use facet::Facet;
use facet_reflect::Peek;
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

#[derive(Facet)]
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

    // Test field values
    let number_value = thing_field.get::<String>().unwrap();
    assert_eq!(*number_value, "12345");
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
    let thing_field = peek_struct
        .field_by_name("thing")
        .expect("Should have a thing field");

    // Test field values
    let proxy_value = thing_field.get::<NotFacetProxy>().unwrap();
    assert_eq!(proxy_value.0, "12345");
}
