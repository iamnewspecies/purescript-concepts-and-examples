// Generated by purs version 0.12.0
"use strict";
var Data_Ring = require("../Data.Ring/index.js");
var Data_Semiring = require("../Data.Semiring/index.js");
var Data_Symbol = require("../Data.Symbol/index.js");
var Data_Unit = require("../Data.Unit/index.js");
var CommutativeRing = function (Ring0) {
    this.Ring0 = Ring0;
};
var CommutativeRingRecord = function (RingRecord0) {
    this.RingRecord0 = RingRecord0;
};
var commutativeRingUnit = new CommutativeRing(function () {
    return Data_Ring.ringUnit;
});
var commutativeRingRecordNil = new CommutativeRingRecord(function () {
    return Data_Ring.ringRecordNil;
});
var commutativeRingRecordCons = function (dictIsSymbol) {
    return function (dictCons) {
        return function (dictCommutativeRingRecord) {
            return function (dictCommutativeRing) {
                return new CommutativeRingRecord(function () {
                    return Data_Ring.ringRecordCons(dictIsSymbol)(dictCons)(dictCommutativeRingRecord.RingRecord0())(dictCommutativeRing.Ring0());
                });
            };
        };
    };
};
var commutativeRingRecord = function (dictRowToList) {
    return function (dictCommutativeRingRecord) {
        return new CommutativeRing(function () {
            return Data_Ring.ringRecord(dictRowToList)(dictCommutativeRingRecord.RingRecord0());
        });
    };
};
var commutativeRingNumber = new CommutativeRing(function () {
    return Data_Ring.ringNumber;
});
var commutativeRingInt = new CommutativeRing(function () {
    return Data_Ring.ringInt;
});
var commutativeRingFn = function (dictCommutativeRing) {
    return new CommutativeRing(function () {
        return Data_Ring.ringFn(dictCommutativeRing.Ring0());
    });
};
module.exports = {
    CommutativeRing: CommutativeRing,
    CommutativeRingRecord: CommutativeRingRecord,
    commutativeRingInt: commutativeRingInt,
    commutativeRingNumber: commutativeRingNumber,
    commutativeRingUnit: commutativeRingUnit,
    commutativeRingFn: commutativeRingFn,
    commutativeRingRecord: commutativeRingRecord,
    commutativeRingRecordNil: commutativeRingRecordNil,
    commutativeRingRecordCons: commutativeRingRecordCons
};
