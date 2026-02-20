//! Base64 - Base64 encoding and decoding for Jet
//!
//! This module provides Base64 encoding and decoding functionality
//! for the Jet programming language.

/// Base64 encoding variants.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Variant {
    /// Standard Base64 (RFC 4648) with padding
    Standard,
    /// URL-safe Base64 (RFC 4648) with padding
    UrlSafe,
    /// Standard Base64 without padding
    StandardNoPad,
    /// URL-safe Base64 without padding
    UrlSafeNoPad,
}

/// Error type for Base64 decoding.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DecodeError {
    message: String,
}

/// Standard Base64 alphabet.
const STANDARD_ALPHABET: &[u8] =
    b"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";

/// URL-safe Base64 alphabet.
const URL_SAFE_ALPHABET: &[u8] =
    b"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_";

/// Padding character.
const PAD: u8 = b'=';

impl Variant {
    /// Returns the alphabet for this variant.
    fn alphabet(&self) -> &'static [u8] {
        match self {
            Variant::Standard | Variant::StandardNoPad => STANDARD_ALPHABET,
            Variant::UrlSafe | Variant::UrlSafeNoPad => URL_SAFE_ALPHABET,
        }
    }

    /// Returns true if this variant uses padding.
    fn uses_padding(&self) -> bool {
        match self {
            Variant::Standard | Variant::UrlSafe => true,
            Variant::StandardNoPad | Variant::UrlSafeNoPad => false,
        }
    }
}

impl DecodeError {
    /// Creates a new decode error.
    pub fn new(message: impl Into<String>) -> Self {
        DecodeError {
            message: message.into(),
        }
    }

    /// Returns the error message.
    pub fn message(&self) -> &str {
        &self.message
    }
}

impl std::fmt::Display for DecodeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Base64 decode error: {}", self.message)
    }
}

impl std::error::Error for DecodeError {}

/// Encodes bytes to a Base64 string using the standard variant.
pub fn encode(input: impl AsRef<[u8]>) -> String {
    encode_config(input, Variant::Standard)
}

/// Encodes bytes to a Base64 string with the specified variant.
pub fn encode_config(input: impl AsRef<[u8]>, variant: Variant) -> String {
    let input = input.as_ref();
    let alphabet = variant.alphabet();
    let use_padding = variant.uses_padding();

    if input.is_empty() {
        return String::new();
    }

    // Calculate output size
    let mut output_len = input.len().div_ceil(3) * 4;
    if !use_padding {
        let remainder = input.len() % 3;
        if remainder != 0 {
            output_len -= 3 - remainder;
        }
    }

    let mut output = Vec::with_capacity(output_len);

    // Process input in chunks of 3 bytes
    for chunk in input.chunks(3) {
        let b0 = chunk[0];
        let b1 = chunk.get(1).copied().unwrap_or(0);
        let b2 = chunk.get(2).copied().unwrap_or(0);

        // Encode 3 bytes to 4 characters
        let c0 = alphabet[((b0 >> 2) & 0x3F) as usize];
        let c1 = alphabet[(((b0 << 4) | (b1 >> 4)) & 0x3F) as usize];
        let c2 = alphabet[(((b1 << 2) | (b2 >> 6)) & 0x3F) as usize];
        let c3 = alphabet[(b2 & 0x3F) as usize];

        output.push(c0);
        output.push(c1);

        if chunk.len() > 1 {
            output.push(c2);
        } else if use_padding {
            output.push(PAD);
        }

        if chunk.len() > 2 {
            output.push(c3);
        } else if use_padding {
            output.push(PAD);
        }
    }

    String::from_utf8(output).expect("Base64 output is valid UTF-8")
}

/// Decodes a Base64 string to bytes using the standard variant.
pub fn decode(input: impl AsRef<str>) -> Result<Vec<u8>, DecodeError> {
    decode_config(input, Variant::Standard)
}

/// Decodes a Base64 string to bytes with the specified variant.
pub fn decode_config(input: impl AsRef<str>, variant: Variant) -> Result<Vec<u8>, DecodeError> {
    let input = input.as_ref();
    let alphabet = variant.alphabet();

    if input.is_empty() {
        return Ok(Vec::new());
    }

    // Build reverse lookup table
    let mut decode_table = [0u8; 256];
    for (i, &c) in alphabet.iter().enumerate() {
        decode_table[c as usize] = i as u8;
    }

    // Count padding characters
    let padding_count = input.chars().rev().take_while(|&c| c == '=').count();
    let data_len = input.len() - padding_count;

    // Calculate output size
    let output_len = data_len * 3 / 4;
    let mut output = Vec::with_capacity(output_len);

    // Process input in chunks of 4 characters
    let chars: Vec<u8> = input.bytes().filter(|&b| b != PAD).collect();

    for chunk in chars.chunks(4) {
        if chunk.len() < 2 {
            return Err(DecodeError::new("Invalid input length"));
        }

        // Decode characters to indices
        let c0 = decode_table[chunk[0] as usize];
        let c1 = decode_table[chunk[1] as usize];

        // Check for invalid characters
        if chunk[0] as usize >= 256
            || decode_table[chunk[0] as usize] == 0 && chunk[0] != alphabet[0]
        {
            if !is_valid_char(chunk[0], variant) {
                return Err(DecodeError::new(format!(
                    "Invalid character: {}",
                    chunk[0] as char
                )));
            }
        }

        // Decode first byte
        output.push((c0 << 2) | (c1 >> 4));

        if chunk.len() > 2 {
            let c2 = decode_table[chunk[2] as usize];
            if !is_valid_char(chunk[2], variant) {
                return Err(DecodeError::new(format!(
                    "Invalid character: {}",
                    chunk[2] as char
                )));
            }
            output.push((c1 << 4) | (c2 >> 2));

            if chunk.len() > 3 {
                let c3 = decode_table[chunk[3] as usize];
                if !is_valid_char(chunk[3], variant) {
                    return Err(DecodeError::new(format!(
                        "Invalid character: {}",
                        chunk[3] as char
                    )));
                }
                output.push((c2 << 6) | c3);
            }
        }
    }

    Ok(output)
}

fn is_valid_char(c: u8, variant: Variant) -> bool {
    match variant {
        Variant::Standard | Variant::StandardNoPad => {
            c.is_ascii_alphanumeric() || c == b'+' || c == b'/'
        }
        Variant::UrlSafe | Variant::UrlSafeNoPad => {
            c.is_ascii_alphanumeric() || c == b'-' || c == b'_'
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_encode_empty() {
        assert_eq!(encode(""), "");
    }

    #[test]
    fn test_encode_basic() {
        assert_eq!(encode("f"), "Zg==");
        assert_eq!(encode("fo"), "Zm8=");
        assert_eq!(encode("foo"), "Zm9v");
        assert_eq!(encode("foob"), "Zm9vYg==");
        assert_eq!(encode("fooba"), "Zm9vYmE=");
        assert_eq!(encode("foobar"), "Zm9vYmFy");
    }

    #[test]
    fn test_encode_standard() {
        let data = b"Hello, World!";
        assert_eq!(encode(data), "SGVsbG8sIFdvcmxkIQ==");
    }

    #[test]
    fn test_encode_url_safe() {
        let data = b"\x00\x01\x02\xfd\xfe\xff";
        assert_eq!(encode_config(data, Variant::Standard), "AAEC/f7/");
        assert_eq!(encode_config(data, Variant::UrlSafe), "AAEC_f7_");
    }

    #[test]
    fn test_encode_no_pad() {
        assert_eq!(encode_config("f", Variant::StandardNoPad), "Zg");
        assert_eq!(encode_config("fo", Variant::StandardNoPad), "Zm8");
        assert_eq!(encode_config("foo", Variant::StandardNoPad), "Zm9v");
    }

    #[test]
    fn test_decode_empty() {
        assert_eq!(decode("").unwrap(), b"");
    }

    #[test]
    fn test_decode_basic() {
        assert_eq!(decode("Zg==").unwrap(), b"f");
        assert_eq!(decode("Zm8=").unwrap(), b"fo");
        assert_eq!(decode("Zm9v").unwrap(), b"foo");
        assert_eq!(decode("Zm9vYg==").unwrap(), b"foob");
        assert_eq!(decode("Zm9vYmE=").unwrap(), b"fooba");
        assert_eq!(decode("Zm9vYmFy").unwrap(), b"foobar");
    }

    #[test]
    fn test_decode_standard() {
        let encoded = "SGVsbG8sIFdvcmxkIQ==";
        assert_eq!(decode(encoded).unwrap(), b"Hello, World!");
    }

    #[test]
    fn test_decode_url_safe() {
        let encoded = "AAEC_f7_";
        assert_eq!(
            decode_config(encoded, Variant::UrlSafe).unwrap(),
            b"\x00\x01\x02\xfd\xfe\xff"
        );
    }

    #[test]
    fn test_decode_no_pad() {
        assert_eq!(decode_config("Zg", Variant::StandardNoPad).unwrap(), b"f");
        assert_eq!(decode_config("Zm8", Variant::StandardNoPad).unwrap(), b"fo");
    }

    #[test]
    fn test_roundtrip() {
        let data = b"The quick brown fox jumps over the lazy dog";
        let encoded = encode(data);
        let decoded = decode(&encoded).unwrap();
        assert_eq!(decoded, data);
    }

    #[test]
    fn test_roundtrip_all_variants() {
        let data = b"\x00\x01\x02\x03\xfd\xfe\xff";

        for variant in [
            Variant::Standard,
            Variant::UrlSafe,
            Variant::StandardNoPad,
            Variant::UrlSafeNoPad,
        ] {
            let encoded = encode_config(data, variant);
            let decoded = decode_config(&encoded, variant).unwrap();
            assert_eq!(decoded, data, "Failed for variant {:?}", variant);
        }
    }

    #[test]
    fn test_decode_invalid_character() {
        assert!(decode("Zm9v!").is_err());
    }

    #[test]
    fn test_binary_data() {
        let data: Vec<u8> = (0..=255).collect();
        let encoded = encode(&data);
        let decoded = decode(&encoded).unwrap();
        assert_eq!(decoded, data);
    }

    #[test]
    fn test_long_string() {
        let data = "a".repeat(1000);
        let encoded = encode(&data);
        let decoded = decode(&encoded).unwrap();
        assert_eq!(String::from_utf8(decoded).unwrap(), data);
    }
}
