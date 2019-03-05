// Generated by purs version 0.12.0
"use strict";
var A = (function () {
    function A() {

    };
    A.value = new A();
    return A;
})();
var B = (function () {
    function B() {

    };
    B.value = new B();
    return B;
})();
var C = (function () {
    function C() {

    };
    C.value = new C();
    return C;
})();
var someFunction = function (input) {
    if (input instanceof A) {
        return "A";
    };
    if (input instanceof B) {
        return "B";
    };
    if (input instanceof C) {
        return "C";
    };
    throw new Error("Failed pattern match at Test line 6, column 22 - line 9, column 10: " + [ input.constructor.name ]);
};
module.exports = {
    A: A,
    B: B,
    C: C,
    someFunction: someFunction
};
