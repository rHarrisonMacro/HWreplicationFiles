function xf = plain_vanilla_project_LSS_model_variables(B,xT,H)
% This module produces a plain-vanilla projection for LSS model variables.
% It projects linear state space (LSS) model variables forward from a set 
% of initial conditions using the state space transition matrix.
%
% INPUTS:
%   -> B: nx*nx matrix of loadings on lagged model variables
%   -> xT: nx*1 vector of initial conditions for model variables
%   -> H: scalar forecast horizon
%
% OUTPUTS
%   -> xf: nx*H matrix of projected model variables
%
% CALLS: 
%   -> generate_and_throw_MAPS_exception
%   -> is_positive_real_integer
%   -> validate_LSS_model_solution_matrices
%   -> interrogate_LSS_model_variable_initial_conditions
%
% DETAILS: 
%   -> This module projects LSS model variables forward given a set of 
%      initial conditions and a law of motion for the evolution of those 
%      variables to produce a model-only projection.
%   -> For a method that can be used to produce a judgemental projection
%      with non-zero projection values for anticipated and/or unanticipated
%      shocks using a LSS model, see the more general 
%      method project_LSS_model_variables.
%
% NOTES:   
%   -> See <> for a description of MAPS forecast modules and macros.
%
% This version: 14/03/2011
% Author(s): Francesca Monti & Matt Waldron

%% CHECK INPUTS
% Check that the number of inputs is as expected. All inputs are 
% compulsory. Check that the forecast horizon input is valid.
if nargin < 3
    errId = ['MAPS:',mfilename,':BadNargin'];
    generate_and_throw_MAPS_exception(errId,{num2str(nargin)});
elseif ~is_positive_real_integer(H)
    errId = ['MAPS:',mfilename,':BadInput3'];
    generate_and_throw_MAPS_exception(errId);
end

%% VALIDATE SOLUTION MATRIX
% Call a MAPS module helper to validate that the loadings on lagged model 
% variables from the solution are as expected.
validate_LSS_model_solution_matrices(B);

%% VALIDATE MODEL VARIABLE INITIAL CONDITIONS
% Call a MAPS module helper to validate that the projection initial 
% conditions are as expected and consistent with the model solution. This
% helper returns the dimension of the model variables.
nx = interrogate_LSS_model_variable_initial_conditions(xT,B);
             
%% PROJECTION
% Project the initial conditions for the model variables forward H periods 
% using the loadings on lagged model variables.
xf = zeros(nx,H);
for s = 1:H
    xf(:,s) = B^s*xT;
end
  
end