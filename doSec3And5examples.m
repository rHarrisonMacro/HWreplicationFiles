% Script to produce simulation experiments using QE model for
% "Optimal policy with occasionally binding constraints: new solution
% algorithms" by Richard Harrison and Matt Waldron.

%% TIDY UP & SET PATHS TO ACCESS MAPS, OBC TOOLKIT AND KEY FOLDERS
tidyUpAndSetPath;

%% EXPERIMENT SPECIFICATION
% Loss function parameters
lambdax = 0.25;
lambdaDeltaR = 1;
lambdaq = 0.05;
lambdaDeltaq = 5;
% Bounds for QE
qLB = -0.001; % Ensures that steady state with q=0 implies no bounds are binding
qUB = 0.5;
shockMnem = 'varepsilon';
shockVal = -(8+3)/4; 
ELBval = -3/4; 
ELBvarMnem = 'Rinst';

% Horizon
H = 150;
% Constraint tolerance
constraintTol = 1e-3;

% Figures
saveFigures = true;
figDims = [12 12]*2; 
figFontSize = 12;

%% BUILD MODEL
Model = create_model('QEmodel.maps');
[xMnems,zMnems] = unpack_model(Model,{'xMnems';'zMnems'});

%% SET OPTIMAL COMMITMENT INFO
% Just policy rate as instrument
boundInfoNoQE.instrumentMnems = ELBvarMnem;
boundInfoNoQE.instrumentCoeffs = 1;
boundInfoNoQE.constants = ELBval;
OptPolInfoNoQE = struct;
OptPolInfoNoQE.policyEqNames = {'Taylor rule'};
OptPolInfoNoQE.policyShockMnems = '';
OptPolInfoNoQE.instrumentMnems = {'Rinst'};
OptPolInfoNoQE.instrumentWeights = 0;
OptPolInfoNoQE.objVarMnems = {'pie';'x';'DeltaR'};
OptPolInfoNoQE.objVarWeights = [1;lambdax;lambdaDeltaR];
OptPolInfoNoQE.beta = 0.9925;
OptPolInfoNoQE.Constraints = boundInfoNoQE;
% QE and policy rate as instrument
boundInfo.instrumentMnems = {ELBvarMnem;'qInst';'qInst'};
boundInfo.instrumentCoeffs = [1; 1; -1];
boundInfo.constants = [ELBval; qLB; -qUB];
OptPolInfo = struct;
OptPolInfo.policyEqNames = {'Taylor rule';'QE rule'};
OptPolInfo.policyShockMnems = '';
OptPolInfo.instrumentMnems = {'Rinst';'qInst'};
OptPolInfo.instrumentWeights = [0;lambdaq];
OptPolInfo.objVarMnems = {'pie';'x';'DeltaR';'Deltaq'};
OptPolInfo.objVarWeights = [1;lambdax;lambdaDeltaR;lambdaDeltaq];
OptPolInfo.beta = 0.9925;
OptPolInfo.Constraints = boundInfo;

%% SOLVE FOR OPTIMAL COMMITMENT & SIMULATE WHEN THERE IS NO QE
[ModelOCnoQE,OptPolInfoNoQE] = ...
    create_LSS_model_with_OC_policy(Model,OptPolInfoNoQE);
[xMnemsOC,zMnemsOC] = unpack_model(ModelOCnoQE,{'xMnems';'zMnems'});
nxOC = size(xMnemsOC,1);
% Compute projection
zInd = lookup_model_index_numbers(zMnemsOC,shockMnem);
nzOC = size(zMnemsOC,1);
antShockValues = zeros(nzOC,H);
antShockValues(zInd,1) = shockVal;
% Overwrite ConstraintInfo with shadow shock mnems
ConstraintInfoNoQE = struct;
ConstraintInfoNoQE.instrumentVarMnems = ELBvarMnem;
ConstraintInfoNoQE.boundVals = ELBval;
ConstraintInfoNoQE.HPopts.fvalTol = constraintTol;
ConstraintInfoNoQE.shadowShockMnems = ...
    OptPolInfoNoQE.Constraints.shadowShockMnems;
x0 = zeros(nxOC,1);
ShocksNoQE = struct;
ShocksNoQE.anticipated = antShockValues;
[xSimulNoQE,areConstraintsBindingNoQE] = ...
    project_LSS_model_variables_subject_to_constraints(ModelOCnoQE,x0,...
    ShocksNoQE,ConstraintInfoNoQE);

%% REPEAT FOR MODEL WITH QE
[ModelOC,OptPolInfo] = create_LSS_model_with_OC_policy(Model,OptPolInfo);
[xMnemsOC,zMnemsOC] = unpack_model(ModelOC,{'xMnems';'zMnems'});
nxOC = size(xMnemsOC,1);
% Compute projection
zInd = lookup_model_index_numbers(zMnemsOC,shockMnem);
nzOC = size(zMnemsOC,1);
antShockValues = zeros(nzOC,H);
antShockValues(zInd,1) = shockVal;
% Add shadow shock mnemonics to ConstraintInfo
ConstraintInfo = struct;
ConstraintInfo.instrumentVarMnems = {ELBvarMnem; 'q' ;'minusq'};
ConstraintInfo.boundVals = [ELBval; qLB; -qUB];
ConstraintInfo.HPopts.fvalTol = constraintTol;
ConstraintInfo.shadowShockMnems = ...
    OptPolInfo.Constraints.shadowShockMnems;
x0 = zeros(nxOC,1);
ShocksQE = struct;
ShocksQE.anticipated = antShockValues;
[xSimul,areConstraintsBindingQE] = ...
    project_LSS_model_variables_subject_to_constraints(ModelOC,x0,...
    ShocksQE,ConstraintInfo);

%% PLOT RESULTS
RstarBarAnn = -4*ELBval; 
varsToPlot =      {'rstarAnn';  'Rann';     'x';'pieYoY';'q';'RcalAnn'};
meanAdjustments = [RstarBarAnn; RstarBarAnn; 0;    0 ;    0 ; RstarBarAnn];
addZeroLine =     [true;       false;      true; false; false; true];
plotRows = 2;
plotCols = 3;
plotH = 24;
xNames = unpack_model(Model,'xNames');
figHandle = figure;
nVarsToPlot = size(varsToPlot,1);
for iVar = 1:nVarsToPlot
    iVarMnem = varsToPlot{iVar};
    iVarInd = lookup_index_numbers_in_string_array(xMnems,iVarMnem);
    subplot(plotRows,plotCols,iVar);
    hold on;
    plot(1:plotH,xSimulNoQE(iVarInd,1:plotH) + meanAdjustments(iVar),...
        'k','linewidth',2);
    plot(1:plotH,xSimul(iVarInd,1:plotH) + meanAdjustments(iVar),...
        'linestyle','--','color',0.5*ones(1,3),'linewidth',2);
    if addZeroLine(iVar)
        plot(1:plotH,zeros(1,plotH),'k','linewidth',1);
    end
    if iVar == nVarsToPlot
        legHandle = legend('Without QE','With QE','Location','East');
        legend boxoff;
        set(legHandle,'Fontsize',figFontSize,'Orientation','Horizontal');
    end
    xlim([1 plotH]);
    set(gca,'Fontsize',figFontSize,'Xtick',4:4:plotH);
    title(xNames{iVarInd},'Fontsize',figFontSize);  
end
% Fix legend placement
legWidth = legHandle.Position(3);
legHeight = legHandle.Position(4);
newLeft = 0.5 - legWidth/2;
newBottom = 0;
legHandle.Position = [newLeft newBottom legWidth legHeight];

%% NOW REPEAT THE EXPERIMENT UNDER OPTIMAL DISCRETION
% Initialise inputs
x0 = zeros(size(xMnems,1),1);
ShocksOD = struct;
ShocksOD.anticipated = zeros(size(zMnems,1),H);
ShocksOD.anticipated(zInd,1) = shockVal;
% First simulate when QE not available
OptPolInfoNoQEOD = OptPolInfoNoQE;
OptPolInfoNoQEOD.Constraints = boundInfoNoQE;
[xSimODnoQE,ODmodelNoQE] = compute_ODPP(Model,OptPolInfoNoQEOD,x0,...
    ShocksOD);
% Now simulate when QE is available
OptPolInfoOD = OptPolInfo;
OptPolInfoOD.Constraints = boundInfo;
[xSimODQE,ODmodelQE] = compute_ODPP(Model,OptPolInfoOD,x0,...
    ShocksOD);

%% PLOT FIGURE FOR OD CASE
ODfigHandle = figure;
nVarsToPlot = size(varsToPlot,1);
for iVar = 1:nVarsToPlot
    iVarMnem = varsToPlot{iVar};
    iVarInd = lookup_index_numbers_in_string_array(xMnems,iVarMnem);
    subplot(plotRows,plotCols,iVar);
    hold on;
    plot(1:plotH,xSimODnoQE(iVarInd,1:plotH) + meanAdjustments(iVar),...
        'k','linewidth',2);
    plot(1:plotH,xSimODQE(iVarInd,1:plotH) + meanAdjustments(iVar),...
        'linestyle','--','color',0.5*ones(1,3),'linewidth',2);
    if addZeroLine(iVar)
        plot(1:plotH,zeros(1,plotH),'k','linewidth',1);
    end
    if iVar == nVarsToPlot
        legHandle = legend('Without QE','With QE','Location','East');
        legend boxoff;
        set(legHandle,'Fontsize',figFontSize,'Orientation','Horizontal');
    end
    xlim([1 plotH]);
    set(gca,'Fontsize',figFontSize,'Xtick',4:4:plotH);
    title(xNames{iVarInd},'Fontsize',figFontSize);  
end
% Fix legend placement
legWidth = legHandle.Position(3);
legHeight = legHandle.Position(4);
newLeft = 0.5 - legWidth/2;
newBottom = 0;
legHandle.Position = [newLeft newBottom legWidth legHeight];


%% SAVE FIGURES IF REQUIRED
if saveFigures
    % Save optimal commitment figure
    figureFileName = [figureFolder '\QEsim.eps'];
    set(figHandle,'PaperUnits','inches','PaperSize',[14 2.75*plotRows+1],...
        'PaperPosition',[1 1 13 2.75*plotRows]);    
    print(figHandle,'-depsc',figureFileName);
    % Save optimal discretion figure
    figureFileName = [figureFolder '\QEsimOD.eps'];
    set(ODfigHandle,'PaperUnits','inches','PaperSize',[14 2.75*plotRows+1],...
        'PaperPosition',[1 1 13 2.75*plotRows]);    
    print(ODfigHandle,'-depsc',figureFileName);    
end
