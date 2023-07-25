%% THIS SCRIPT SETS UP THE ENVIRONMENT TO GENERATE THE EXAMPLES IN
% 'OPTIMAL POLICY WITH OCCASIONALLY BINDING CONSTRAINTS: PIECEWISE LINEAR
% SOLUTION METHODS', BY RICHARD HARRISON & MATT WALDRON

%% TIDY WORKSPACE, FIGURES AND COMMAND WINDOW
clear variables;
close all;
clc;
delete('*.asv');

%% SPECIFY LOCATIONS OF CODEBASE TO BE USED
MAPScodeDir = 'MAPSlite';
commitmentCodeDir = 'Toolkit\Commitment';
helperCodeDir = 'Toolkit\Helpers';
HPcodeDir = 'Toolkit\HoldenPaetz';
discretionCodeDir = 'Toolkit\Discretion';
nonPolOBCcodeDir =  'Toolkit\NonPolicyOBCs';

%% SPECIFY FOLDERS FOR FIGURES AND RESULTS SAVING
resultsFolder = 'Results\';
figureFolder = 'Figures';

%% SET PATHS
restoredefaultpath;
addpath(genpath(MAPScodeDir));
addpath(genpath(helperCodeDir));
addpath(genpath(commitmentCodeDir));
addpath(genpath(HPcodeDir));
addpath(genpath(discretionCodeDir));
addpath(genpath(nonPolOBCcodeDir));
addpath('ModelsAndAddOns');
addpath('Helpers');