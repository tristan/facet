use facet::Facet;
use facet_reflect::{HasFields, Peek, ProxyPeek};
use facet_testhelpers::test;

#[derive(Debug)]
struct NotFacet {
    val: u64,
}

impl From<String> for NotFacet {
    fn from(s: String) -> NotFacet {
        NotFacet {
            val: s.parse().unwrap(),
        }
    }
}

impl From<&NotFacet> for String {
    fn from(nf: &NotFacet) -> String {
        format!("{}", nf.val)
    }
}

#[derive(Facet, PartialEq)] // PartialEq only needed for the test
#[facet(transparent)]
struct NotFacetProxy(String);

impl From<NotFacetProxy> for NotFacet {
    fn from(value: NotFacetProxy) -> Self {
        value.0.into()
    }
}

impl From<&NotFacet> for NotFacetProxy {
    fn from(value: &NotFacet) -> Self {
        NotFacetProxy(value.into())
    }
}

#[derive(Facet, Debug)]
struct TestStruct {
    #[facet(proxy(String))]
    thing: NotFacet,
}

#[derive(Facet, Debug)]
struct TestNewTypeStruct {
    #[facet(proxy(NotFacetProxy))]
    thing: NotFacet,
}

#[test]
fn peek_struct_with_proxy() {
    // Create test struct instance
    let test_struct = TestStruct {
        thing: NotFacet { val: 12345 },
    };
    let peek_value = Peek::new(&test_struct);

    // Convert to struct and check we can convert to PeekStruct
    let peek_struct = peek_value
        .into_struct()
        .expect("Should be convertible to struct");

    // Test field access by name
    let thing_field = peek_struct
        .field_by_name("thing")
        .expect("Should have a thing field");

    let mut tested = false;
    for (field, peek) in peek_struct.fields_for_serialize() {
        tested = true;
        assert_eq!(thing_field, peek);
        assert!(field.has_proxy());
        let proxy = ProxyPeek::from_peek(peek, field).expect("should support proxy");
        // Test field values
        let peek = proxy.as_peek();
        let proxy_value = peek.get::<String>().unwrap();
        assert_eq!(*proxy_value, "12345");
    }
    assert!(tested);
}

#[test]
fn peek_struct_with_proxy_newtype() {
    // Create test struct instance
    let test_struct = TestNewTypeStruct {
        thing: NotFacet { val: 12345 },
    };
    let peek_value = Peek::new(&test_struct);

    // Convert to struct and check we can convert to PeekStruct
    let peek_struct = peek_value
        .into_struct()
        .expect("Should be convertible to struct");

    // Test field access by name
    let thing_peek = peek_struct
        .field_by_name("thing")
        .expect("Should have a thing field");

    let mut tested = false;
    for (field, peek) in peek_struct.fields_for_serialize() {
        tested = true;
        assert_eq!(thing_peek, peek);
        assert!(field.has_proxy());
        let proxy = ProxyPeek::from_peek(peek, field).expect("should support proxy");
        // Test field values
        let peek = proxy.as_peek();
        let proxy_value = peek.get::<NotFacetProxy>().unwrap();
        assert_eq!(proxy_value.0, "12345");
    }
    assert!(tested);
}
