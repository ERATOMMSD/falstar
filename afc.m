addpath 'src/test/simulink/'
InitBreach
fuel_inj_tol = 1.0
MAF_sensor_tol = 1.0
AF_sensor_tol = 1.0
pump_tol = 1.0
kappa_tol = 1.0
tau_ww_tol = 1.0
fault_time = 60.0
kp = 0.04
ki = 0.14
sys = BreachSimulinkSystem('fuel_control')
sys.Sys.tspan = 0:50.0
phi = STL_Formula('(alw_[11.0,50.0] ((((throttle[t] < 8.8) and (ev_[0.0,0.05] (40.0 < throttle[t]))) or ((40.0 < throttle[t]) and (ev_[0.0,0.05] (throttle[t] < 8.8)))) => (alw_[1.0,5.0] (abs(mu[t]) < 0.008))))', '(alw_[11.0,50.0] ((((throttle[t] < 8.8) and (ev_[0.0,0.05] (40.0 < throttle[t]))) or ((40.0 < throttle[t]) and (ev_[0.0,0.05] (throttle[t] < 8.8)))) => (alw_[1.0,5.0] (abs(mu[t]) < 0.008))))')
gen_throttle = fixed_cp_signal_gen({'throttle'}, 10)
gen_engine = constant_signal_gen({'engine'})
gen = BreachSignalGen({gen_throttle, gen_engine})
sys.SetInputGen(gen)
sys.SetParamRanges({'throttle_u0'}, [0.0 61.1])
sys.SetParamRanges({'throttle_u1'}, [0.0 61.1])
sys.SetParamRanges({'throttle_u2'}, [0.0 61.1])
sys.SetParamRanges({'throttle_u3'}, [0.0 61.1])
sys.SetParamRanges({'throttle_u4'}, [0.0 61.1])
sys.SetParamRanges({'throttle_u5'}, [0.0 61.1])
sys.SetParamRanges({'throttle_u6'}, [0.0 61.1])
sys.SetParamRanges({'throttle_u7'}, [0.0 61.1])
sys.SetParamRanges({'throttle_u8'}, [0.0 61.1])
sys.SetParamRanges({'throttle_u9'}, [0.0 61.1])
sys.SetParamRanges({'engine_u0'}, [900.0 1100.0])
problem = FalsificationProblem(sys, phi)
problem.max_obj_eval = 100
problem.max_time = 1000
problem.setup_solver('cmaes')
problem.solver_options.Seed = 0
problem.solve()
time = problem.time_spent
sims = problem.nb_obj_eval
score = problem.obj_best
best = problem.BrSet_Best