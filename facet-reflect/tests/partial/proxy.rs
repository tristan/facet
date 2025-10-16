use facet::Facet;
use facet_reflect::Partial;
use facet_testhelpers::IPanic;

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
fn proxy_string_field() -> Result<(), IPanic> {
    let mut partial = Partial::alloc::<TestStruct>().unwrap();

    // Set the thing field
    partial.begin_field("thing")?;
    partial.set("12345".to_string())?;
    partial.end()?; // end thing field

    let built = *partial.build()?;

    // Verify the result
    assert_eq!(built.thing.val, 12345);

    Ok(())
}

#[test]
fn proxy_newtype_string_field() -> Result<(), IPanic> {
    let mut partial = Partial::alloc::<TestNewTypeStruct>().unwrap();

    // Set the thing field
    partial.begin_field("thing")?;
    partial.set(NotFacetProxy("12345".to_string()))?;
    partial.end()?; // end thing field

    let built = *partial.build()?;

    // Verify the result
    assert_eq!(built.thing.val, 12345);

    Ok(())
}
