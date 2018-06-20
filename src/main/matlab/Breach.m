function [score, sims, time, t__, u__] = Breach(model, inits, inputs, phi, T, solver, stages, samples)

sys = BreachSimulinkSystem(model.name);
gen.type = 'UniStep';

ni = numel(inputs);
dt = model.dt;

gen.cp = stages;
sys.SetInputGen(gen);
sys.Sys.tspan = 0:dt:T;

samples = samples * stages;

% prepare input signal
for k = 1:stages
	% set a single input
	for i = 1:ni
		in = inputs(i);
		sig = strcat(in.name, '_u', num2str(k-1));
		sys.SetParamRanges({sig}, in.range);
	end
end

% optimize
problem = FalsificationProblem(sys, phi);

problem.max_obj_eval = samples;
problem.max_time = 600; % ten minutes should be enough
problem.setup_solver(solver);

% random seed for cmaes
problem.solver_options.Seed = randi(1000);

problem.solve();

% collect some statistics
time = problem.time_spent;
sims = problem.nb_obj_eval;
score = problem.obj_best;

% read out falsifying input signal
best = problem.BrSet_Best;

t = 0;
t__ = zeros(stages, 1);
u__ = zeros(stages, ni);

for k = 1:stages
    t__(k,1) = t;
    t = t + (T / stages);
        
	for i = 1:ni
		in = inputs(i);
		sig = strcat(in.name, '_u', num2str(k-1));
		u = best.GetParam(sig);
        u__(k,i) = u;
	end
end
	
end
