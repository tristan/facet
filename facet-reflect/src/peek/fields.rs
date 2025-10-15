use core::ops::Range;

use facet_core::{Field, FieldFlags, GenericPtr};

use crate::Peek;
use alloc::{vec, vec::Vec};

use super::{PeekEnum, PeekStruct, PeekTuple};

/// Trait for types that have field methods
///
/// This trait allows code to be written generically over both structs and enums
/// that provide field access and iteration capabilities.
pub trait HasFields<'mem, 'facet> {
    /// Iterates over all fields in this type, providing both field metadata and value
    fn fields(&self) -> FieldIter<'mem, 'facet>;

    /// Iterates over fields in this type that should be included when it is serialized
    fn fields_for_serialize(&self) -> FieldsForSerializeIter<'mem, 'facet> {
        FieldsForSerializeIter {
            stack: vec![self.fields()],
            to_drop: vec![],
        }
    }
}

/// An iterator over all the fields of a struct or enum. See [`HasFields::fields`]
pub struct FieldIter<'mem, 'facet> {
    state: FieldIterState<'mem, 'facet>,
    range: Range<usize>,
}

enum FieldIterState<'mem, 'facet> {
    Struct(PeekStruct<'mem, 'facet>),
    Tuple(PeekTuple<'mem, 'facet>),
    Enum {
        peek_enum: PeekEnum<'mem, 'facet>,
        fields: &'static [Field],
    },
    FlattenedEnum {
        field: Field,
        value: Peek<'mem, 'facet>,
    },
}

impl<'mem, 'facet> FieldIter<'mem, 'facet> {
    #[inline]
    pub(crate) fn new_struct(struct_: PeekStruct<'mem, 'facet>) -> Self {
        Self {
            range: 0..struct_.ty.fields.len(),
            state: FieldIterState::Struct(struct_),
        }
    }

    #[inline]
    pub(crate) fn new_enum(enum_: PeekEnum<'mem, 'facet>) -> Self {
        // Get the fields of the active variant
        let variant = match enum_.active_variant() {
            Ok(v) => v,
            Err(e) => panic!("Cannot get active variant: {e:?}"),
        };
        let fields = &variant.data.fields;

        Self {
            range: 0..fields.len(),
            state: FieldIterState::Enum {
                peek_enum: enum_,
                fields,
            },
        }
    }

    #[inline]
    pub(crate) fn new_tuple(tuple: PeekTuple<'mem, 'facet>) -> Self {
        Self {
            range: 0..tuple.len(),
            state: FieldIterState::Tuple(tuple),
        }
    }

    fn get_field_by_index(&self, index: usize) -> Option<(Field, Peek<'mem, 'facet>)> {
        match self.state {
            FieldIterState::Struct(peek_struct) => {
                let field = peek_struct.ty.fields.get(index).copied()?;
                let value = peek_struct.field(index).ok()?;
                Some((field, value))
            }
            FieldIterState::Tuple(peek_tuple) => {
                let field = peek_tuple.ty.fields.get(index).copied()?;
                let value = peek_tuple.field(index)?;
                Some((field, value))
            }
            FieldIterState::Enum { peek_enum, fields } => {
                // Get the field definition
                let field = fields[index];
                // Get the field value
                let field_value = match peek_enum.field(index) {
                    Ok(Some(v)) => v,
                    Ok(None) => return None,
                    Err(e) => panic!("Cannot get field: {e:?}"),
                };
                // Return the field definition and value
                Some((field, field_value))
            }
            FieldIterState::FlattenedEnum { field, value } => {
                if index == 0 {
                    Some((field, value))
                } else {
                    None
                }
            }
        }
    }
}

impl<'mem, 'facet> Iterator for FieldIter<'mem, 'facet> {
    type Item = (Field, Peek<'mem, 'facet>);

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let index = self.range.next()?;

            let Some(field) = self.get_field_by_index(index) else {
                continue;
            };

            return Some(field);
        }
    }

    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        self.range.size_hint()
    }
}

impl DoubleEndedIterator for FieldIter<'_, '_> {
    #[inline]
    fn next_back(&mut self) -> Option<Self::Item> {
        loop {
            let index = self.range.next_back()?;

            let Some(field) = self.get_field_by_index(index) else {
                continue;
            };

            return Some(field);
        }
    }
}

impl ExactSizeIterator for FieldIter<'_, '_> {}

/// An iterator over the fields of a struct or enum that should be serialized. See [`HasFields::fields_for_serialize`]
pub struct FieldsForSerializeIter<'mem, 'facet> {
    stack: Vec<FieldIter<'mem, 'facet>>,
    // used to keep track of fields that have owned data, so it can be dropped
    // TODO: this is certainly a bad idea: the peek value could be used after the iterator has been dropped
    // e.g. if doing something like fields().collect()
    to_drop: Vec<(Field, GenericPtr<'mem>)>,
}

impl<'mem, 'facet> Iterator for FieldsForSerializeIter<'mem, 'facet> {
    type Item = (Field, Peek<'mem, 'facet>);

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let mut fields = self.stack.pop()?;
            let Some((mut field, peek)) = fields.next() else {
                continue;
            };
            self.stack.push(fields);

            let ptr = peek.data();
            let Some(data) = ptr.thin() else {
                continue;
            };
            let should_skip = unsafe { field.should_skip_serializing(data) };

            if field.vtable.drop_serialize_into_box.is_some() {
                self.to_drop.push((field, ptr));
            }

            if should_skip {
                continue;
            }

            if field.flags.contains(FieldFlags::FLATTEN) && !field.flattened {
                if let Ok(struct_peek) = peek.into_struct() {
                    self.stack.push(FieldIter::new_struct(struct_peek))
                } else if let Ok(enum_peek) = peek.into_enum() {
                    // normally we'd serialize to something like:
                    //
                    //   {
                    //     "field_on_struct": {
                    //       "VariantName": { "field_on_variant": "foo" }
                    //     }
                    //   }
                    //
                    // But since `field_on_struct` is flattened, instead we do:
                    //
                    //   {
                    //     "VariantName": { "field_on_variant": "foo" }
                    //   }
                    field.name = enum_peek
                        .active_variant()
                        .expect("Failed to get active variant")
                        .name;
                    field.flattened = true;
                    self.stack.push(FieldIter {
                        range: 0..1,
                        state: FieldIterState::FlattenedEnum { field, value: peek },
                    });
                } else {
                    // TODO: fail more gracefully
                    panic!("cannot flatten a {}", field.shape())
                }
            } else {
                return Some((field, peek));
            }
        }
    }
}

impl<'mem, 'facet> Drop for FieldsForSerializeIter<'mem, 'facet> {
    fn drop(&mut self) {
        while let Some((field, ptr)) = self.to_drop.pop() {
            if let Some(drop_fn) = field.vtable.drop_serialize_into_box {
                unsafe { drop_fn(ptr) };
            }
        }
    }
}
