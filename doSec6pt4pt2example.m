% THIS SCRIPT PRODUCES RESULTS FOR SECTION 6.4.2 OF 
% 'OPTIMAL POLICY WITH OCCASIONALLY BINDING CONSTRAINTS: NEW SOLUTION
% ALGORITHMS', BY RICHARD HARRISON & MATT WALDRON

%% TIDY UP & SET PATHS TO ACCESS MAPS, OBC TOOLKIT AND KEY FOLDERS
tidyUpAndSetPath;

%% RESULTS/FIGURE SAVING OPTIONS
saveFigures = true;

%% EXPERIMENT OPTIONS
simHorizon = 50;
shockMnem = {'varepsilon'};
newsPeriod = 4; 
shockSize = -2;
lambda = 0.25; % Output gap loss weight 

%% MODEL & OPTIMAL POLICY OPTIONS
modelName = 'INK.maps';
policyEqName = 'Taylor rule';
policyVarMnem = 'i';
policyVarWeight = 0;
policyShockMnem = '';
objVarMnems = {'pie';'x'};
objVarWeights = [1;lambda];
optPolBetaToEvaluate = 'p.beta'; 

%% BUILD THE MODEL
Model = create_model(modelName);

%% EXTRACT PARAMETER VALUES FROM MODEL
p = unpack_model_parameter_values_as_struct(Model);

%% ANALYSE MODEL CONTENTS
[xMnems,zMnems,xNames] = unpack_model(Model,{'xMnems';'zMnems';'xNames'});
nx = size(xMnems,1);
nz = size(zMnems,1);
shkInd = lookup_model_index_numbers(zMnems,shockMnem);

%% CONSTRUCT OPTIMAL POLICY INFORMATION STRUCTURE
optPolInfo = struct;
optPolInfo.policyEqNames = policyEqName;
optPolInfo.policyShockMnems = policyShockMnem;
optPolInfo.instrumentMnems = policyVarMnem;
optPolInfo.instrumentWeights = policyVarWeight;
optPolInfo.objVarMnems = objVarMnems;
optPolInfo.objVarWeights = objVarWeights;
optPolInfo.beta = eval(optPolBetaToEvaluate);

%% OPTIMAL POLICY WITH NO CONSTRAINTS
x0 = zeros(nx,1);
Shocks.anticipated = zeros(nz,simHorizon);
Shocks.anticipated(shkInd,newsPeriod) = shockSize;

xSimUncon = compute_ODPP(Model,optPolInfo,x0,Shocks);

%% AUGMENT PROBLEM TO IMPOSE ZLB ON POLICY RATE
% Specify (quarterly) inflation target
piStar = 0.5;
% Infer ZLB for policy rate
ELBval = 100*(1-(1+piStar/100)/p.beta);
Constraints.instrumentMnems = 'i';
Constraints.instrumentCoeffs = 1;
Constraints.constants = ELBval;
optPolInfoZLB = optPolInfo;
optPolInfoZLB.Constraints = Constraints;

xSimZLBonly = compute_ODPP(Model,optPolInfoZLB,x0,Shocks);

%% APPLY NON-INSTRUMENT OBCs
Options = struct;
Options.solutionUpdateMethod = 'fullUpdateInactiveFirst';
Options.maxIter = 10;
nonPolOBCs = struct;
nonPolOBCs.inactive.VarMnems = {'wtilde'};
nonPolOBCs.inactive.EqNames = {'Labour supply curve'};
nonPolOBCs.active.VarMnems = {'piw'};
nonPolOBCs.active.Constants = -piStar;
 
%% ANALYSE MODEL AND EXTEND NON-POLICY OBC STRUCTURE
nonPolOBCs = ...
    add_model_info_to_non_pol_OBC_struct(nonPolOBCs,Model,optPolInfoZLB);

%% RUN THE SIMULATION
[xSim,OBCsAreActiveSim,diagnostics] = ...
    project_LSS_model_under_OD_subject_to_non_policy_constraints(...
    Model,x0,Shocks,optPolInfoZLB,nonPolOBCs,Options);

%% PLOT OPTIONS
varMnemsToPlot = {'x';'pie';  'iAnn';   'w';'piw';'wtilde'};
constants =      [0;  piStar; -4*ELBval; 0; piStar; 0 ];
nCols = 3;
plotHorizon = 24;
figDims = [12 12]*2;  %[12 16]*1.5;
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
    hold on;
    plot(1:plotHorizon,...
        iConstant+xSimUncon(plotVarInds(iVar),1:plotHorizon),...
        'linestyle','--','color',0.5*ones(1,3),'linewidth',2);
    plot(1:plotHorizon,...
        iConstant+xSimZLBonly(plotVarInds(iVar),1:plotHorizon),...
        'linestyle','-','color',0.5*ones(1,3),'linewidth',2);
    plot(1:plotHorizon,...
        iConstant+xSim(plotVarInds(iVar),1:plotHorizon),...
        'color','k','linewidth',2);
	ylims = get(gca,'yLim');
    if ylims(1)>-tinyTol && ylims(2)<tinyTol
        set(gca,'yLim',[-1 1]);
    end
    set(gca,'xLim',[1 plotHorizon],'fontSize',figFontSize);
    title(plotNames{iVar},'fontSize',figFontSize);
end
legHandle = legend('Unconstrained','ZLB and flexible wages',...
    'ZLB and downward nominal wage rigidity');
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
    figureFileName = [figureFolder '\ODwithNonPolOBCs.eps'];
    set(nonPolOBChandle,'PaperUnits','inches','PaperSize',...
        [14 2.75*nRows+1],'PaperPosition',[1 1 13 2.75*nRows]);    
    print(nonPolOBChandle,'-depsc',figureFileName);    
end
