use facet::Facet;
use facet_testhelpers::test;

#[test]
fn test_transparent_newtype_const_generic() {
    #[derive(Facet)]
    #[facet(transparent)]
    struct FromStrRadix<const RADIX: u32>(String);

    impl<const RADIX: u32> TryFrom<FromStrRadix<RADIX>> for u64 {
        type Error = std::num::ParseIntError;

        fn try_from(value: FromStrRadix<RADIX>) -> Result<Self, Self::Error> {
            u64::from_str_radix(&value.0, RADIX)
        }
    }

    impl From<&u64> for FromStrRadix<16> {
        fn from(value: &u64) -> Self {
            Self(format!("{value:x}"))
        }
    }

    impl From<&u64> for FromStrRadix<10> {
        fn from(value: &u64) -> Self {
            Self(format!("{value}"))
        }
    }

    impl From<&u64> for FromStrRadix<8> {
        fn from(value: &u64) -> Self {
            Self(format!("{value:o}"))
        }
    }

    #[derive(facet::Facet, Debug)]
    pub struct Main {
        #[facet(proxy = FromStrRadix<16>)]
        hex: u64,
        #[facet(proxy = FromStrRadix<8>)]
        oct: u64,
        #[facet(proxy = FromStrRadix<10>)]
        dec: u64,
    }

    let data = r#"{"hex":"7b","oct":"173","dec":"123"}"#;

    let test: Main = facet_json::from_str(data).unwrap();
    assert_eq!(test.hex, 123);
    assert_eq!(test.oct, 123);
    assert_eq!(test.dec, 123);
    assert_eq!(facet_json::to_string(&test), data);
}
