#lang reader "sl.rkt"


genExpr expr = match expr {
  core.Quote val => genConstant val
}


genConstant val = match isNumber val {
  True => genNumber val
}

genNumber n = numberToString n


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



import core
