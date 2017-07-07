use super::*;

#[test]
fn basics() {
    let cases = vec![
        ("  1 +   1", 2.0),
        (" 4 * 7 - 14", 14.0),
        (" 2 << 16 ", 131072.0),
        (" ((4 * 18) % 17) / 3", 4.0 / 3.0),
        ("2²³²", 4096.0),
        ("4 ^ 3 ^ 2 ^ 3 ^ 4 ^ 2", 0.0),
        ("3 << (4 >> 2)", 6.0),
    ];
    for (input, expected) in cases {
        assert_eq!(eval(input), Ok(expected));
    }
}

#[test]
fn tokens() {
    let line = "(3 + 7) >> 10 * (7 % 2)";
    let expected = vec![
        Token::OpenParen,
        Token::Number(3.0),
        Token::Plus,
        Token::Number(7.0),
        Token::CloseParen,
        Token::BitWiseRShift,
        Token::Number(10.0),
        Token::Multiply,
        Token::OpenParen,
        Token::Number(7.0),
        Token::Modulo,
        Token::Number(2.0),
        Token::CloseParen,
    ];
    assert_eq!(tokenize(line), Ok(expected));
}

#[test]
fn random() {
    let cases = vec![
        (
            "((15 * 10) - 26 * 19 - 30 / ((57 * 79 + 93 / 87 / 47))) / 8",
            -43.00083277394169075309
        ),
        ("(3 << 6) * 7 + (40 / 3)", 1357.33333333333333333333),
        ("(21 & (5) ^ (20 & 81)) / (25 << 3)", 0.105),
        (
            "(79 & 14) * ((3) - 76 + 67 / (62) - (85 ^ (7 - (32) >> 52)))",
            197.12903225806448
        ),
    ];

    for (input, expected) in cases {
        assert_eq!(eval(input), Ok(expected));
    }
}
