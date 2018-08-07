// Generated by purs version 0.12.0
"use strict";
var Control_Applicative = require("../Control.Applicative/index.js");
var Control_Apply = require("../Control.Apply/index.js");
var Control_Bind = require("../Control.Bind/index.js");
var Control_Monad = require("../Control.Monad/index.js");
var Control_Monad_Cont_Class = require("../Control.Monad.Cont.Class/index.js");
var Control_Monad_Reader_Class = require("../Control.Monad.Reader.Class/index.js");
var Control_Monad_State_Class = require("../Control.Monad.State.Class/index.js");
var Control_Monad_Trans_Class = require("../Control.Monad.Trans.Class/index.js");
var Control_Semigroupoid = require("../Control.Semigroupoid/index.js");
var Data_Function = require("../Data.Function/index.js");
var Data_Functor = require("../Data.Functor/index.js");
var Data_Newtype = require("../Data.Newtype/index.js");
var Effect_Class = require("../Effect.Class/index.js");
var Prelude = require("../Prelude/index.js");
var ContT = function (x) {
    return x;
};
var withContT = function (f) {
    return function (v) {
        return function (k) {
            return v(f(k));
        };
    };
};
var runContT = function (v) {
    return function (k) {
        return v(k);
    };
};
var newtypeContT = new Data_Newtype.Newtype(function (n) {
    return n;
}, ContT);
var monadTransContT = new Control_Monad_Trans_Class.MonadTrans(function (dictMonad) {
    return function (m) {
        return function (k) {
            return Control_Bind.bind(dictMonad.Bind1())(m)(k);
        };
    };
});
var mapContT = function (f) {
    return function (v) {
        return function (k) {
            return f(v(k));
        };
    };
};
var functorContT = function (dictFunctor) {
    return new Data_Functor.Functor(function (f) {
        return function (v) {
            return function (k) {
                return v(function (a) {
                    return k(f(a));
                });
            };
        };
    });
};
var applyContT = function (dictApply) {
    return new Control_Apply.Apply(function () {
        return functorContT(dictApply.Functor0());
    }, function (v) {
        return function (v1) {
            return function (k) {
                return v(function (g) {
                    return v1(function (a) {
                        return k(g(a));
                    });
                });
            };
        };
    });
};
var bindContT = function (dictBind) {
    return new Control_Bind.Bind(function () {
        return applyContT(dictBind.Apply0());
    }, function (v) {
        return function (k) {
            return function (k$prime) {
                return v(function (a) {
                    var v1 = k(a);
                    return v1(k$prime);
                });
            };
        };
    });
};
var applicativeContT = function (dictApplicative) {
    return new Control_Applicative.Applicative(function () {
        return applyContT(dictApplicative.Apply0());
    }, function (a) {
        return function (k) {
            return k(a);
        };
    });
};
var monadContT = function (dictMonad) {
    return new Control_Monad.Monad(function () {
        return applicativeContT(dictMonad.Applicative0());
    }, function () {
        return bindContT(dictMonad.Bind1());
    });
};
var monadAskContT = function (dictMonadAsk) {
    return new Control_Monad_Reader_Class.MonadAsk(function () {
        return monadContT(dictMonadAsk.Monad0());
    }, Control_Monad_Trans_Class.lift(monadTransContT)(dictMonadAsk.Monad0())(Control_Monad_Reader_Class.ask(dictMonadAsk)));
};
var monadReaderContT = function (dictMonadReader) {
    return new Control_Monad_Reader_Class.MonadReader(function () {
        return monadAskContT(dictMonadReader.MonadAsk0());
    }, function (f) {
        return function (v) {
            return function (k) {
                return Control_Bind.bind(((dictMonadReader.MonadAsk0()).Monad0()).Bind1())(Control_Monad_Reader_Class.ask(dictMonadReader.MonadAsk0()))(function (v1) {
                    return Control_Monad_Reader_Class.local(dictMonadReader)(f)(v(function ($45) {
                        return Control_Monad_Reader_Class.local(dictMonadReader)(Data_Function["const"](v1))(k($45));
                    }));
                });
            };
        };
    });
};
var monadContContT = function (dictMonad) {
    return new Control_Monad_Cont_Class.MonadCont(function () {
        return monadContT(dictMonad);
    }, function (f) {
        return function (k) {
            var v = f(function (a) {
                return function (v1) {
                    return k(a);
                };
            });
            return v(k);
        };
    });
};
var monadEffectContT = function (dictMonadEffect) {
    return new Effect_Class.MonadEffect(function () {
        return monadContT(dictMonadEffect.Monad0());
    }, function ($46) {
        return Control_Monad_Trans_Class.lift(monadTransContT)(dictMonadEffect.Monad0())(Effect_Class.liftEffect(dictMonadEffect)($46));
    });
};
var monadStateContT = function (dictMonadState) {
    return new Control_Monad_State_Class.MonadState(function () {
        return monadContT(dictMonadState.Monad0());
    }, function ($47) {
        return Control_Monad_Trans_Class.lift(monadTransContT)(dictMonadState.Monad0())(Control_Monad_State_Class.state(dictMonadState)($47));
    });
};
module.exports = {
    ContT: ContT,
    runContT: runContT,
    mapContT: mapContT,
    withContT: withContT,
    newtypeContT: newtypeContT,
    monadContContT: monadContContT,
    functorContT: functorContT,
    applyContT: applyContT,
    applicativeContT: applicativeContT,
    bindContT: bindContT,
    monadContT: monadContT,
    monadTransContT: monadTransContT,
    monadEffectContT: monadEffectContT,
    monadAskContT: monadAskContT,
    monadReaderContT: monadReaderContT,
    monadStateContT: monadStateContT
};
