extern crate test;

use super::eval;
use test::Bencher;

const SEXPR: &str = "4 ** (88 * 9 / (59 >> 3))";
const MEXPR: &str = "((((87))) - 73) + (97 + (((15 / 55 * ((31)) + 35))) + (15 - (9)) - (39 / 26) / 20 / 91 + 27 / (33 * 26 + 28 - (7) / 10 + 66 * 6) + 60 / 35 - ((29) - (69) / 44 / (92)) / (89) + 2 + 87 / 47 * ((2)) * 83 / 98 * 42 / (((67)) * ((97))) / (34 / 89 + 77) - 29 + 70 * (20)) + ((((((92))) + 23 * (98) / (95) + (((99) * (41))) + (5 + 41) + 10) - (36) / (6 + 80 * 52 + (90))))";

#[bench]
fn small_expr(bench: &mut Bencher) {
    bench.iter(|| eval(SEXPR));
}

#[bench]
fn medium_expr(bench: &mut Bencher) {
    bench.iter(|| eval(MEXPR));
}
