InitBreach;

fuel_inj_tol = 1.0; 
MAF_sensor_tol = 1.0;
AF_sensor_tol = 1.0; 
pump_tol = 1.;
kappa_tol=1; 
tau_ww_tol=1;
fault_time=60;
kp = 0.04;
ki = 0.14;

addpath 'src/test/simulink/'

pedal_in = fixed_cp_signal_gen({'Pedal_Angle'}, 10);
engine_in = constant_signal_gen({'Engine_Speed'});

gen = BreachSignalGen({pedal_in, engine_in});

sys = BreachSimulinkSystem('fuel_control');
sys.SetInputGen(gen);

sys.SetParamRanges({'Engine_Speed_u0'}, [900 1100]);
sys.SetParamRanges({'Pedal_Angle_u0'}, [0.0 61.1]);
sys.SetParamRanges({'Pedal_Angle_u1'}, [0.0 61.1]);
sys.SetParamRanges({'Pedal_Angle_u2'}, [0.0 61.1]);
sys.SetParamRanges({'Pedal_Angle_u3'}, [0.0 61.1]);
sys.SetParamRanges({'Pedal_Angle_u4'}, [0.0 61.1]);
sys.SetParamRanges({'Pedal_Angle_u5'}, [0.0 61.1]);
sys.SetParamRanges({'Pedal_Angle_u6'}, [0.0 61.1]);
sys.SetParamRanges({'Pedal_Angle_u7'}, [0.0 61.1]);
sys.SetParamRanges({'Pedal_Angle_u8'}, [0.0 61.1]);
sys.SetParamRanges({'Pedal_Angle_u9'}, [0.0 61.1]);

phi = STL_Formula('phi', '(alw_[11.0,50.0] ((((Pedal_Angle[t] < 8.8) and (ev_[0.0,0.05] (40.0 < Pedal_Angle[t]))) or ((40.0 < Pedal_Angle[t]) and (ev_[0.0,0.05] (Pedal_Angle[t] < 8.8)))) => (alw_[1.0,5.0] (abs(mu[t]) < 0.008))))');
problem = FalsificationProblem(sys, phi);
problem.max_obj_eval = 100;
problem.setup_solver('cmaes');
problem.solve();

time = problem.time_spent
sims = problem.nb_obj_eval
score = problem.obj_best

