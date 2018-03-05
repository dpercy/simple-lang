#lang reader "sl.rkt"


genExpr expr = match expr {
  core.Quote val => genConstant val
}


genConstant val = match isNumber val {
  True => genNumber val
}

genNumber n = match is32Bit n {
  True => numberToString n
  False => "error(\"number not representable in 32 bits\")"
}

is32Bit n = and (lessEq (sub 0 (expt 2 31)) n) (less n (expt 2 31))
check is32Bit 42 = True
check is32Bit 2147483647 = True
check is32Bit 2147483648 = False
check is32Bit (sub 0 2147483648) = True
check is32Bit (sub 0 2147483649) = False

numberToString n = match less n 10 {
  True => digitToString n
  False => {
    hi = numberToString (div n 10)
    low = digitToString (mod n 10)
    strcat hi low
  }
}
digitToString n = chr (add n (ord "0"))
check digitToString 0 = "0"
check digitToString 7 = "7"
check numberToString 123 = "123"
check genNumber 123456789 = "123456789"

# TODO if JS can't represent "too-big" numbers, how can it self host?
#  - maybe add arith operators that wrap
#  - maybe add arith operators that return error values
#  - maybe add an explicit bignum type - different from these "best-effort integers"
check genNumber 123456789123456789 = "error(\"number not representable in 32 bits\")"
check genNumber (sub 0 123456789123456789) = "error(\"number not representable in 32 bits\")"



import core
