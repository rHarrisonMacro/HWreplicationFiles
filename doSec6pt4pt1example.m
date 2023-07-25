%% THIS SCRIPT PRODUCES RESULTS FOR SECTION 6.4.2 OF 
% 'OPTIMAL POLICY WITH OCCASIONALLY BINDING CONSTRAINTS: NEW SOLUTION
% ALGORITHMS', BY RICHARD HARRISON & MATT WALDRON

%% TIDY UP & SET PATHS TO ACCESS MAPS, OBC TOOLKIT AND KEY FOLDERS
tidyUpAndSetPath;

%% CHOOSE WHETHER TO SAVE FIGURES
saveFigures = true;

%% EXPERIMENT OPTIONS
simHorizon = 50;
newsPeriod = 4; 
shockMnem = {'eb'}; 
shockSize = -2;

%% MODEL & OPTIMAL POLICY OPTIONS
modelName = 'SmetsWouters2007.maps';

policyEqName = 'Monetary policy rule';
policyVarMnem = 'r';
policyVarWeight = 0;
policyShockMnem = '';
FITobjVarMnems = {'pinf';'ygap'};
FITobjVarWeights = [1;0.25];
optPolBetaToEvaluate = 'p.cbeta'; 

%% BUILD THE MODEL
% Create model structure
Model = create_model(modelName);
% Reparameterise
Model = update_parameters_and_resolve_LSS_model(Model,0.85,'crhob');
% Update to incorporate investment speed limits
Model = modify_model_from_struct(Model,'updateInvestmentStructure.maps');

%% EXTRACT PARAMETER VALUES FROM MODEL
p = unpack_model_parameter_values_as_struct(Model);

%% ANALYSE MODEL CONTENTS
[xMnems,zMnems,xNames] = unpack_model(Model,{'xMnems';'zMnems';'xNames'});
nx = size(xMnems,1);
nz = size(zMnems,1);
shkInd = lookup_model_index_numbers(zMnems,shockMnem);

%% OPTIMAL POLICY FOR FIT LOSS FUNCTION
% Build inputs
x0 = zeros(nx,1);
Shocks.anticipated = zeros(nz,simHorizon);
Shocks.anticipated(shkInd,newsPeriod) = shockSize;
% Optimal policy info structure
OptPolInfo = struct;
OptPolInfo.policyEqNames = policyEqName;
OptPolInfo.policyShockMnems = policyShockMnem;
OptPolInfo.instrumentMnems = policyVarMnem;
OptPolInfo.instrumentWeights = policyVarWeight;
OptPolInfo.objVarMnems = FITobjVarMnems;
OptPolInfo.objVarWeights = FITobjVarWeights;
OptPolInfo.beta = eval(optPolBetaToEvaluate);

%% AUGMENT PROBLEM TO IMPOSE ZLB ON POLICY RATE
ELBval = -p.conster;
Constraints.instrumentMnems = 'r';
Constraints.instrumentCoeffs = 1;
Constraints.constants = ELBval;
OptPolInfoZLB = OptPolInfo;
OptPolInfoZLB.Constraints = Constraints;

%% CREATE VARIANT OF MODEL WITH OPTIMAL COMMITMENT POLICY
H = simHorizon;
[ModelOC,OptPolInfoOC] = ...
    create_LSS_model_with_OC_policy(Model,OptPolInfoZLB);

%% SIMULATE WITHOUT ELB
[BOC,PHIOC,FOC,xMnemsOC,zMnemsOC] = ...
    unpack_model(ModelOC,{'B';'PHI';'F';'xMnems';'zMnems'});
nxOC = size(xMnemsOC,1);
x0OC = zeros(nxOC,1);

zIndOC = lookup_model_index_numbers(zMnemsOC,shockMnem);
nzOC = size(zMnemsOC,1);
antShockValues = zeros(nzOC,H);
antShockValues(zIndOC,newsPeriod) = shockSize;

ShocksOC = struct;
ShocksOC.anticipated = antShockValues;

xSimNoZLB = project_LSS_model_variables(BOC,PHIOC,FOC,x0OC,ShocksOC);

%% SIMULATE WITH ELB
ConstraintInfo = struct;
ConstraintInfo.instrumentVarMnems = 'r';
ConstraintInfo.boundVals = ELBval;
ConstraintInfo.HPopts.fvalTol = 1e-05;
ConstraintInfo.shadowShockMnems = ...
    OptPolInfoOC.Constraints.shadowShockMnems;

[xSimZLB,areConstraintsBinding] = ...
    project_LSS_model_variables_subject_to_constraints(ModelOC,x0OC,...
    ShocksOC,ConstraintInfo);

%% CONSTRUCT INFORMATION ABOUT NON-POLICY OBCS
nonPolOBCs = struct;
nonPolOBCs.inactive.VarMnems = {'zeta';'xi'};
nonPolOBCs.inactive.EqNames = ...
    {'Lagrange multiplier for lower speed limit on investment';...
    'Lagrange multiplier for upper speed limit on investment'};
nonPolOBCs.active.VarMnems = {'dinve';'negdinve'};
nonPolOBCs.active.Constants = [-4-p.ctrend;-4+p.ctrend];
 
%% ADD MODEL-SPECIFIC INFORMATION TO NON-POLICY OBC STRUCTURE
nonPolOBCs = ...
    add_model_info_to_non_pol_OBC_struct(nonPolOBCs,Model,OptPolInfoZLB);

%% RUN THE SIMULATION
[xSimOC,OBCsAreActiveSim,diagnostics] = ...
    project_LSS_model_under_OC_subject_to_non_policy_constraints(...
    Model,x0OC,Shocks,OptPolInfoZLB,nonPolOBCs); 

%% PLOT OPTIONS
varMnemsToPlot = {'ygap';'pinf';      'r';       'inve'; 'dinve'};
constants =      [0;     p.constepinf; p.conster;   0;     p.ctrend ]; 
scaling =        [1;      1;           4;           1;      1];
nCols = 3;
plotHorizon = 24;
figDims = [12 12]*2; 
figFontSize = 12;
% Analyse options
plotVarInds = lookup_model_index_numbers(xMnems,varMnemsToPlot);
plotNames = xNames(plotVarInds);
nVarsToPlot = size(varMnemsToPlot,1);
nRows = ceil(nVarsToPlot/nCols);
tinyTol = 1e-9;

%% COMPARATIVE PLOT
nonPolOBChandle = figure;
for iVar = 1:nVarsToPlot
    subplot(nRows,nCols,iVar);
    iConstant = constants(iVar);
    iScaling = scaling(iVar);
    hold on;
    plot(1:plotHorizon,...
        iScaling*(iConstant+xSimNoZLB(plotVarInds(iVar),1:plotHorizon)),...
        'linestyle','--','color',0.5*ones(1,3),'linewidth',2);
    plot(1:plotHorizon,...
        iScaling*(iConstant+xSimZLB(plotVarInds(iVar),1:plotHorizon)),...
        'linestyle','-','color',0.5*ones(1,3),'linewidth',2);
    plot(1:plotHorizon,...
        iScaling*(iConstant+xSimOC(plotVarInds(iVar),1:plotHorizon)),...
        'color','k','linewidth',2);
	ylims = get(gca,'yLim');
    if ylims(1)>-tinyTol && ylims(2)<tinyTol
        set(gca,'yLim',[-1 1]);
    end
    set(gca,'xLim',[1 plotHorizon],'fontSize',figFontSize);
    title(plotNames{iVar},'fontSize',figFontSize);
end
legHandle = ...
    legend('Unconstrained','ZLB only','ZLB and investment speed limits');
legend boxoff;
legHandle.Orientation = 'horizontal';
legHandle.FontSize = figFontSize;
% Fix legend placement
legWidth = legHandle.Position(3);
legHeight = legHandle.Position(4);
newLeft = 0.5 - legWidth/2;
newBottom = 0;
legHandle.Position = [newLeft newBottom legWidth legHeight];
% Ad hoc addition of additional panel with multipliers
subplot(nRows,nCols,nVarsToPlot+1)
hold on;
xiInd = lookup_model_index_numbers(xMnems,'xi');
zetaInd = lookup_model_index_numbers(xMnems,'zeta');
plot(1:plotHorizon,xSimOC(xiInd,1:plotHorizon),'k:','linewidth',2);
plot(1:plotHorizon,xSimOC(zetaInd,1:plotHorizon),'k-.','linewidth',2);
set(gca,'xLim',[1 plotHorizon],'fontSize',figFontSize);
title('Multipliers on non-policy OBCs','fontSize',figFontSize);
legend('\xi','\zeta','Location','NorthEast','Fontsize',12);
legend boxoff;

%% SAVE FIGURES IF REQUIRED
if saveFigures
    figureFileName = [figureFolder '\OCwithNonPolOBCs.eps'];
    set(nonPolOBChandle,'PaperUnits','inches','PaperSize',[14 2.75*nRows+1],...
        'PaperPosition',[1 1 13 2.75*nRows]);    
    print(nonPolOBChandle,'-depsc',figureFileName);    
end
