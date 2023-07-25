%% THIS SCRIPT PRODUCES RESULTS FOR SECTION [X] OF 
% 'OPTIMAL POLICY WITH OCCASIONALLY BINDING CONSTRAINTS: NEW SOLUTION
% ALGORITHMS', BY RICHARD HARRISON & MATT WALDRON

%% TIDY UP & SET PATHS TO ACCESS MAPS, OBC TOOLKIT AND KEY FOLDERS
tidyUpAndSetPath;

%% EXPERIMENT OPTIONS
simHorizon = 50;
newsPeriod = 6;  
shockMnem = {'etah'};
shockSize = 225; 

%% MODEL & OPTIMAL POLICY OPTIONS
modelName = 'FHN2018.maps';
% Info about policy rule, instrument and shock
epsilon = 6; 
policyEqName = 'Monetary policy rule';
policyVarMnem = 'i';
policyVarWeight = 0;
policyShockMnem = '';
FITobjVarMnems = {'pie';'x'};
FITobjVarWeightsToEvaluate = {'epsilon/p.gamma';'1'};
LAWobjVarMnems = {'pie';'x';'cgap';'hgap'};
LAWobjVarWeightsToEvaluate = {'epsilon/p.gamma';'1';...
    'p.xi*(1-p.xi)*p.sigma*(1+p.sigma+p.phi)/((1+p.phi)*(p.sigma+p.phi))';...
    'p.sigmah*p.xi*(1-p.xi)/(p.sigma+p.phi)'};
optPolBetaToEvaluate = 'p.betas'; 

%% BUILD & AUGMENT THE MODEL
Model = create_model(modelName);

%% EXTRACT PARAMETER VALUES FROM MODEL
p = unpack_model_parameter_values_as_struct(Model);

%% COMPUTE KEY PARAMETERS FOR EXPERIMENTS
ELBval = 100*(1-1/p.betas);

%% PLOT OPTIONS
varMnemsToPlot = {'x';'pie';'iAnn';'hgap';'cgap';'q'};
constants =      [ 0;  0;   -4*ELBval; 0;   0;    0];
nCols = 3;
plotHorizon = 40;
saveFigures = true;
figFontSize = 12;
% Analyse options
nVarsToPlot = size(varMnemsToPlot,1);
nRows = ceil(nVarsToPlot/nCols);
tinyTol = 1e-9;

%% ANALYSE MODEL CONTENTS
[xMnems,zMnems,xNames] = unpack_model(Model,{'xMnems';'zMnems';'xNames'});
nx = size(xMnems,1);
nz = size(zMnems,1);
shkInd = lookup_model_index_numbers(zMnems,shockMnem);
plotVarInds = lookup_model_index_numbers(xMnems,varMnemsToPlot);
plotNames = xNames(plotVarInds);

%% OPTIMAL POLICY FOR FIT LOSS FUNCTION
% Build inputs
x0 = zeros(nx,1);
Shocks.anticipated = zeros(nz,simHorizon);
Shocks.anticipated(shkInd,newsPeriod) = shockSize;
% Optimal policy info structure
FITinfo = struct;
FITinfo.policyEqNames = policyEqName;
FITinfo.policyShockMnems = policyShockMnem;
FITinfo.instrumentMnems = policyVarMnem;
FITinfo.instrumentWeights = policyVarWeight;
FITinfo.objVarMnems = FITobjVarMnems;
nFITobjVars = size(FITobjVarWeightsToEvaluate,1);
FITobjVarWeights = nan(nFITobjVars,1);
for iWeight = 1:nFITobjVars
    FITobjVarWeights(iWeight) = eval(FITobjVarWeightsToEvaluate{iWeight});
end
FITinfo.objVarWeights = FITobjVarWeights;
FITinfo.beta = eval(optPolBetaToEvaluate);
% Construct the projection
[xfFIT,ODinfoFIT] = compute_ODPP(Model,FITinfo,x0,Shocks);

%% BUILD THE "NAIVE" VARIANT
ODmodelFIT = create_LSS_model_with_discretion_targeting_rule(Model,FITinfo,...
    'Targeting rule',false,'','');
[BDFIT,PHIDFIT,HCDFIT,HFDFIT] = ...
    unpack_model(ODmodelFIT,{'B';'PHI';'HC';'HF'});
FDFIT = -(HCDFIT+HFDFIT*BDFIT)\HFDFIT;
xfNaiveFIT = project_LSS_model_variables(BDFIT,PHIDFIT,FDFIT,x0,Shocks);

%% PLOT FIT RESULTS
FIThandle = figure;
for iVar = 1:nVarsToPlot
    subplot(nRows,nCols,iVar);
    hold on;
    iConstant = constants(iVar);
    plot(1:plotHorizon,iConstant+xfFIT(plotVarInds(iVar),1:plotHorizon),...
        'color','k','linewidth',2);
    plot(1:plotHorizon,iConstant+xfNaiveFIT(plotVarInds(iVar),1:plotHorizon),...
        'linestyle','--','color',0.5*ones(1,3),'linewidth',2);
    ylims = get(gca,'yLim');
    if ylims(1)>-tinyTol && ylims(2)<tinyTol
        set(gca,'yLim',[-1 1]);
    end
    set(gca,'xLim',[1 plotHorizon],'fontSize',figFontSize);
    title(plotNames{iVar},'fontSize',figFontSize);
end
legHandle = legend('True solution','"Naive" approach');
legend boxoff;
legHandle.Orientation = 'horizontal';
legHandle.FontSize = figFontSize;
% Fix legend placement
legWidth = legHandle.Position(3);
legHeight = legHandle.Position(4);
newLeft = 0.5 - legWidth/2;
newBottom = 0;
legHandle.Position = [newLeft newBottom legWidth legHeight];


%% OPTIMAL POLICY FOR LAW LOSS FUNCTION
% Optimal policy info structure
LAWinfo = struct;
LAWinfo.policyEqNames = policyEqName;
LAWinfo.policyShockMnems = policyShockMnem;
LAWinfo.instrumentMnems = policyVarMnem;
LAWinfo.instrumentWeights = policyVarWeight;
LAWinfo.objVarMnems = LAWobjVarMnems;
nLAWobjVars = size(LAWobjVarWeightsToEvaluate,1);
LAWobjVarWeights = nan(nLAWobjVars,1);
for iWeight = 1:nLAWobjVars
    LAWobjVarWeights(iWeight) = eval(LAWobjVarWeightsToEvaluate{iWeight});
end
LAWinfo.objVarWeights = LAWobjVarWeights;
LAWinfo.beta = eval(optPolBetaToEvaluate);
% Construct the projection
[xfLAW,ODinfoLAW] = compute_ODPP(Model,LAWinfo,x0,Shocks);


%% BUILD THE "NAIVE" VARIANT
ODmodelLAW = create_LSS_model_with_discretion_targeting_rule(Model,LAWinfo,...
    'Targeting rule',false,'','');
[BDLAW,PHIDLAW,HCDLAW,HFDLAW] = ...
    unpack_model(ODmodelLAW,{'B';'PHI';'HC';'HF'});
FDLAW = -(HCDLAW+HFDLAW*BDLAW)\HFDLAW;
xfNaiveLAW = project_LSS_model_variables(BDLAW,PHIDLAW,FDLAW,x0,Shocks);

%% PLOT LAW RESULTS
LAWhandle = figure;
for iVar = 1:nVarsToPlot
    subplot(nRows,nCols,iVar);
    hold on;
    iConstant = constants(iVar);
    plot(1:plotHorizon,iConstant + xfLAW(plotVarInds(iVar),1:plotHorizon),...
        'color','k','linewidth',2);
    plot(1:plotHorizon,iConstant + xfNaiveLAW(plotVarInds(iVar),1:plotHorizon),...
        'linestyle','--','color',0.5*ones(1,3),'linewidth',2);
        ylims = get(gca,'yLim');
    if ylims(1)>-tinyTol && ylims(2)<tinyTol
        set(gca,'yLim',[-1 1]);
    end
    set(gca,'xLim',[1 plotHorizon],'fontSize',figFontSize);
    title(plotNames{iVar},'fontSize',figFontSize);
end
legHandle = legend('True solution','"Naive" approach');
legend boxoff;
legHandle.Orientation = 'horizontal';
legHandle.FontSize = figFontSize;
% Fix legend placement
legWidth = legHandle.Position(3);
legHeight = legHandle.Position(4);
newLeft = 0.5 - legWidth/2;
newBottom = 0;
legHandle.Position = [newLeft newBottom legWidth legHeight];

%% SAVE FIGURES IF REQUIRED
if saveFigures
    % Save FIT figure
    figureFileName = [figureFolder '\FHNsimFIT.eps'];
    set(FIThandle,'PaperUnits','inches','PaperSize',[14 2.75*nRows+1],...
        'PaperPosition',[1 1 13 2.75*nRows]);    
    print(FIThandle,'-depsc',figureFileName);
    % Save LAW figure
    figureFileName = [figureFolder '\FHNsimLAW.eps'];
    set(LAWhandle,'PaperUnits','inches','PaperSize',[14 2.75*nRows+1],...
        'PaperPosition',[1 1 13 2.75*nRows]);    
    print(LAWhandle,'-depsc',figureFileName);     
end

