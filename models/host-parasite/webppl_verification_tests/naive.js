var webppl = require("/usr/local/lib/node_modules/webppl/src/main.js");
var args = require("/usr/local/lib/node_modules/webppl/src/args.js");
args.makeGlobal(__filename, process.argv.slice(2));
var __runner__ = util.trampolineRunners.cli();
function topK(s, x) {
  console.log(x);
};
var main = (function (_globalCurrentAddress) {
    return function (p) {
        return function (runTrampoline) {
            return function (s, k, a) {
                runTrampoline(function () {
                    return p(s, k, a);
                });
            };
        };
    }(function (globalStore, _k0, _address0) {
        var _currentAddress = _address0;
        _addr.save(_globalCurrentAddress, _address0);
        var Bernoulli = dists.makeBernoulli;
        var Beta = dists.makeBeta;
        var Binomial = dists.makeBinomial;
        var Categorical = dists.makeCategorical;
        var Cauchy = dists.makeCauchy;
        var Delta = dists.makeDelta;
        var DiagCovGaussian = dists.makeDiagCovGaussian;
        var Dirichlet = dists.makeDirichlet;
        var Discrete = dists.makeDiscrete;
        var Exponential = dists.makeExponential;
        var Gamma = dists.makeGamma;
        var Gaussian = dists.makeGaussian;
        var ImproperUniform = dists.makeImproperUniform;
        var IspNormal = dists.makeIspNormal;
        var KDE = dists.makeKDE;
        var Laplace = dists.makeLaplace;
        var LogisticNormal = dists.makeLogisticNormal;
        var LogitNormal = dists.makeLogitNormal;
        var Marginal = dists.makeMarginal;
        var Mixture = dists.makeMixture;
        var Multinomial = dists.makeMultinomial;
        var MultivariateBernoulli = dists.makeMultivariateBernoulli;
        var MultivariateGaussian = dists.makeMultivariateGaussian;
        var Poisson = dists.makePoisson;
        var RandomInteger = dists.makeRandomInteger;
        var SampleBasedMarginal = dists.makeSampleBasedMarginal;
        var TensorGaussian = dists.makeTensorGaussian;
        var TensorLaplace = dists.makeTensorLaplace;
        var Uniform = dists.makeUniform;
        var condition = function condition(globalStore, _k160, _address120, bool) {
            var _currentAddress = _address120;
            _addr.save(_globalCurrentAddress, _address120);
            var _k162 = function (globalStore, _result161) {
                _addr.save(_globalCurrentAddress, _currentAddress);
                return function () {
                    return factor(globalStore, _k160, _address120.concat('_150'), _result161);
                };
            };
            return function () {
                return bool ? _k162(globalStore, 0) : _k162(globalStore, ad.scalar.neg(Infinity));
            };
        };
        var error = function error(globalStore, _k159, _address121, msg) {
            var _currentAddress = _address121;
            _addr.save(_globalCurrentAddress, _address121);
            return function () {
                return _k159(globalStore, util.error(msg));
            };
        };
        var SampleGuide = function SampleGuide(globalStore, _k155, _address125, wpplFn, options) {
            var _currentAddress = _address125;
            _addr.save(_globalCurrentAddress, _address125);
            return function () {
                return ForwardSample(globalStore, _k155, _address125.concat('_154'), wpplFn, _.assign({ guide: !0 }, _.omit(options, 'guide')));
            };
        };
        var OptimizeThenSample = function OptimizeThenSample(globalStore, _k153, _address126, wpplFn, options) {
            var _currentAddress = _address126;
            _addr.save(_globalCurrentAddress, _address126);
            return function () {
                return Optimize(globalStore, function (globalStore, _dummy154) {
                    _addr.save(_globalCurrentAddress, _currentAddress);
                    var opts = _.pick(options, 'samples', 'onlyMAP', 'verbose');
                    return function () {
                        return SampleGuide(globalStore, _k153, _address126.concat('_156'), wpplFn, opts);
                    };
                }, _address126.concat('_155'), wpplFn, _.omit(options, 'samples', 'onlyMAP'));
            };
        };
        var DefaultInfer = function DefaultInfer(globalStore, _k143, _address127, wpplFn, options) {
            var _currentAddress = _address127;
            _addr.save(_globalCurrentAddress, _address127);
            var _dummy152 = util.mergeDefaults(options, {}, 'Infer');
            var maxEnumTreeSize = 200000;
            var minSampleRate = 250;
            var samples = 1000;
            return function () {
                return Enumerate(globalStore, function (globalStore, enumResult) {
                    _addr.save(_globalCurrentAddress, _currentAddress);
                    var _k151 = function (globalStore, _dummy150) {
                        _addr.save(_globalCurrentAddress, _currentAddress);
                        var _dummy149 = console.log('Using "rejection"');
                        return function () {
                            return Rejection(globalStore, function (globalStore, rejResult) {
                                _addr.save(_globalCurrentAddress, _currentAddress);
                                return function () {
                                    return rejResult instanceof Error ? function (globalStore, _dummy148) {
                                        _addr.save(_globalCurrentAddress, _currentAddress);
                                        return function () {
                                            return CheckSampleAfterFactor(globalStore, function (globalStore, hasSampleAfterFactor) {
                                                _addr.save(_globalCurrentAddress, _currentAddress);
                                                var _k146 = function (globalStore, _dummy145) {
                                                    _addr.save(_globalCurrentAddress, _currentAddress);
                                                    var _dummy144 = console.log('Using "MCMC"');
                                                    return function () {
                                                        return MCMC(globalStore, _k143, _address127.concat('_163'), wpplFn, { samples: samples });
                                                    };
                                                };
                                                return function () {
                                                    return hasSampleAfterFactor ? function (globalStore, _dummy147) {
                                                        _addr.save(_globalCurrentAddress, _currentAddress);
                                                        return function () {
                                                            return SMC(globalStore, function (globalStore, smcResult) {
                                                                _addr.save(_globalCurrentAddress, _currentAddress);
                                                                return function () {
                                                                    return dists.isDist(smcResult) ? _k143(globalStore, smcResult) : smcResult instanceof Error ? _k146(globalStore, console.log(ad.scalar.add(smcResult.message, '..quit SMC'))) : error(globalStore, _k146, _address127.concat('_162'), 'Invalid return value from SMC');
                                                                };
                                                            }, _address127.concat('_161'), wpplFn, {
                                                                throwOnError: !1,
                                                                particles: samples
                                                            });
                                                        };
                                                    }(globalStore, console.log('Using "SMC" (interleaving samples and factors detected)')) : _k146(globalStore, undefined);
                                                };
                                            }, _address127.concat('_160'), wpplFn);
                                        };
                                    }(globalStore, console.log(ad.scalar.add(rejResult.message, '..quit rejection'))) : dists.isDist(rejResult) ? _k143(globalStore, rejResult) : error(globalStore, _k143, _address127.concat('_164'), 'Invalid return value from rejection');
                                };
                            }, _address127.concat('_159'), wpplFn, {
                                minSampleRate: minSampleRate,
                                throwOnError: !1,
                                samples: samples
                            });
                        };
                    };
                    return function () {
                        return dists.isDist(enumResult) ? _k143(globalStore, enumResult) : enumResult instanceof Error ? _k151(globalStore, console.log(ad.scalar.add(enumResult.message, '..quit enumerate'))) : error(globalStore, _k151, _address127.concat('_158'), 'Invalid return value from enumerate');
                    };
                }, _address127.concat('_157'), wpplFn, {
                    maxEnumTreeSize: maxEnumTreeSize,
                    maxRuntimeInMS: 5000,
                    throwOnError: !1,
                    strategy: 'depthFirst'
                });
            };
        };
        var Infer = function Infer(globalStore, _k136, _address128, options, maybeFn) {
            var _currentAddress = _address128;
            _addr.save(_globalCurrentAddress, _address128);
            var _k142 = function (globalStore, wpplFn) {
                _addr.save(_globalCurrentAddress, _currentAddress);
                var _k141 = function (globalStore, _dummy140) {
                    _addr.save(_globalCurrentAddress, _currentAddress);
                    var methodMap = {
                        SMC: SMC,
                        MCMC: MCMC,
                        PMCMC: PMCMC,
                        asyncPF: AsyncPF,
                        rejection: Rejection,
                        enumerate: Enumerate,
                        incrementalMH: IncrementalMH,
                        forward: ForwardSample,
                        optimize: OptimizeThenSample,
                        defaultInfer: DefaultInfer
                    };
                    var _k139 = function (globalStore, methodName) {
                        _addr.save(_globalCurrentAddress, _currentAddress);
                        var _k138 = function (globalStore, _dummy137) {
                            _addr.save(_globalCurrentAddress, _currentAddress);
                            var method = methodMap[methodName];
                            return function () {
                                return method(globalStore, _k136, _address128.concat('_167'), wpplFn, _.omit(options, 'method', 'model'));
                            };
                        };
                        return function () {
                            return _.has(methodMap, methodName) ? _k138(globalStore, undefined) : function (globalStore, methodNames) {
                                _addr.save(_globalCurrentAddress, _currentAddress);
                                var msg = ad.scalar.add(ad.scalar.add(ad.scalar.add(ad.scalar.add('Infer: \'', methodName), '\' is not a valid method. The following methods are available: '), methodNames.join(', ')), '.');
                                return function () {
                                    return error(globalStore, _k138, _address128.concat('_166'), msg);
                                };
                            }(globalStore, _.keys(methodMap));
                        };
                    };
                    return function () {
                        return options.method ? _k139(globalStore, options.method) : _k139(globalStore, 'defaultInfer');
                    };
                };
                return function () {
                    return _.isFunction(wpplFn) ? _k141(globalStore, undefined) : error(globalStore, _k141, _address128.concat('_165'), 'Infer: a model was not specified.');
                };
            };
            return function () {
                return util.isObject(options) ? maybeFn ? _k142(globalStore, maybeFn) : _k142(globalStore, options.model) : _k142(globalStore, options);
            };
        };
        var mymodel = function mymodel(globalStore, _k13, _address163) {
            var _currentAddress = _address163;
            _addr.save(_globalCurrentAddress, _address163);
            var r_0 = 3;
            var r_1 = 1;
            var start_state = 0;
            var stop_state = 1;
            var start_time = 1;
            var stop_time = 0;
            return function () {
                return sim_event(globalStore, function (globalStore, n_events) {
                    _addr.save(_globalCurrentAddress, _currentAddress);
                    return function () {
                        return _k13(globalStore, n_events);
                    };
                }, _address163.concat('_252'), start_state, stop_state, start_time, stop_time, r_0, r_1);
            };
        };
        var sim_event = function sim_event(globalStore, _k5, _address164, state, stop_state, start_time, stop_time, r_0, r_1) {
            var _currentAddress = _address164;
            _addr.save(_globalCurrentAddress, _address164);
            var _k12 = function (globalStore, rate) {
                _addr.save(_globalCurrentAddress, _currentAddress);
                return function () {
                    return Exponential(globalStore, function (globalStore, _result11) {
                        _addr.save(_globalCurrentAddress, _currentAddress);
                        return function () {
                            return sample(globalStore, function (globalStore, t) {
                                _addr.save(_globalCurrentAddress, _currentAddress);
                                var new_time = ad.scalar.sub(start_time, t);
                                var _k8 = function (globalStore, _dummy7) {
                                    _addr.save(_globalCurrentAddress, _currentAddress);
                                    return function () {
                                        return sim_event(globalStore, function (globalStore, _result6) {
                                            _addr.save(_globalCurrentAddress, _currentAddress);
                                            return function () {
                                                return _k5(globalStore, ad.scalar.add(1, _result6));
                                            };
                                        }, _address164.concat('_256'), ad.scalar.sub(1, state), stop_state, new_time, stop_time, r_0, r_1);
                                    };
                                };
                                return function () {
                                    return ad.scalar.lt(new_time, stop_time) ? function (globalStore, _k10) {
                                        _addr.save(_globalCurrentAddress, _currentAddress);
                                        return function () {
                                            return ad.scalar.neq(state, stop_state) ? condition(globalStore, _k10, _address164.concat('_255'), ad.scalar.eq(0, 1)) : _k10(globalStore, undefined);
                                        };
                                    }(globalStore, function (globalStore, _dummy9) {
                                        _addr.save(_globalCurrentAddress, _currentAddress);
                                        return function () {
                                            return _k5(globalStore, 0);
                                        };
                                    }) : _k8(globalStore, undefined);
                                };
                            }, _address164.concat('_254'), _result11);
                        };
                    }, _address164.concat('_253'), { a: rate });
                };
            };
            return function () {
                return ad.scalar.eq(state, 0) ? _k12(globalStore, r_0) : _k12(globalStore, r_1);
            };
        };
        return function () {
            return Infer(globalStore, function (globalStore, dist) {
                _addr.save(_globalCurrentAddress, _currentAddress);
                return function () {
                    return _k0(globalStore, dist.normalizationConstant);
                };
            }, _address0.concat('_261'), {
                method: 'SMC',
                particles: 10000,
                model: mymodel
            });
        };
    });
});

webppl.runEvaled(main, __runner__, {}, {}, topK, '');