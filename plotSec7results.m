%% THIS SCRIPT PLOTS RESULTS FOR SECTION 7 OF 
% 'OPTIMAL POLICY WITH OCCASIONALLY BINDING CONSTRAINTS: NEW SOLUTION
% ALGORITHMS', BY RICHARD HARRISON & MATT WALDRON

%% TIDY UP & SET PATHS TO ACCESS MAPS, OBC TOOLKIT AND KEY FOLDERS
tidyUpAndSetPath;

%% OPTIONS
saveFigure = true; 
resultsFileName = 'Results\FRBUS.mat';
xVarsToPlot = {'lur';'inflation';'rffe'};
adjustmentMnems = {'ustar';'infTar';'FFRbar'};
yLims = {[4,10]; [0.5,3]; [0,5]};
varNames = {'Unemployment rate, %';'Annual PCE inflation, %';...
    'Effective federal funds rate, %'};
figFontSize = 12;
nBackDataPeriods = 12;
plotHorizon = 28; 
nRows = 1; 
dataStartDate = 1985; % The full data set begins in 1985Q1

%% LOAD RESULTS
load(resultsFileName,'Model','ModelOC','ModelOD','xSimul','xSimulOD',...
        'baselineForecast');

%% GATHER INFORMATION FOR THE PLOT
[theta,thetaMnems] = unpack_model(Model,{'theta';'thetaMnems'});
adjustmentValues = ...
    theta(lookup_model_index_numbers(thetaMnems,adjustmentMnems));
nVarsToPlot = size(xVarsToPlot,1);
nCols = ceil(nVarsToPlot/nRows);
xInds = unpack_model_metadata_and_lookup_index_numbers(Model,...
    'xMnems',xVarsToPlot);
nanBackData = nan(1,nBackDataPeriods-1);
T = size(baselineForecast.Past.modelVariables,2);
plotEndDate = dataStartDate + (T+plotHorizon)/4;
plotStartDate = plotEndDate - (plotHorizon+nBackDataPeriods-1)/4;
plotDates = plotStartDate:0.25:plotEndDate;
vlineDate = plotDates(nBackDataPeriods);

%% PLOT RESULTS
figHandle = figure;
for iVar = 1:nVarsToPlot
    subplot(nRows,nCols,iVar);
    hold on;
    iInd = xInds(iVar);
    iAdj = adjustmentValues(iVar);
    iBackData = baselineForecast.Past.modelVariables(...
        iInd,end-nBackDataPeriods+1:end);
    iBaseForecast = baselineForecast.Forecast.modelVariables(...
        iInd,1:plotHorizon);
    iOCforecast = xSimul(iInd,1:plotHorizon);
    OCline = ...
        plot(plotDates,[nanBackData, iBackData(end), iOCforecast]+iAdj,...
        'linestyle','-','color',0.5*ones(1,3),'linewidth',2);
	iODforecast = xSimulOD(iInd,1:plotHorizon);
    ODline = ...
        plot(plotDates,[nanBackData, iBackData(end), iODforecast]+iAdj,...
        'linestyle','--',...
        'color',0.5*ones(1,3),'linewidth',2);
    baseline = plot(plotDates,[iBackData iBaseForecast]+iAdj,...
        'k','linewidth',2);
    iYlims = yLims{iVar};
    plot([vlineDate vlineDate],[iYlims(1) iYlims(2)],'k--','linewidth',1);
    xlim([plotDates(1) plotDates(end)]);
    title(varNames{iVar},'Fontsize',figFontSize);
    set(gca,'Fontsize',figFontSize);
    if iVar == nVarsToPlot
        legHandle = legend([baseline,OCline,ODline],'Baseline',...
        'Commitment','Discretion');
        legend boxoff;
        legHandle.FontSize = figFontSize;
        legHandle.Location = 'NorthEast';
    end
end

%% SAVE FIGURE
if saveFigure
    figFileName = [figureFolder '\FRBUS.eps'];
    set(figHandle,'PaperUnits','inches','PaperSize',[14 2.75*nRows+1],...
        'PaperPosition',[1 1 13 2.75*nRows]);    
    print(figHandle,'-depsc',figFileName);    
end

%% COMPUTE LOSSES
OptPolInfo.beta = 0.9925;
optBeta = OptPolInfo.beta;
objVarMnems = {'inflation';'lur';'DeltaR'};
objVarInds = unpack_model_metadata_and_lookup_index_numbers(Model,...
    'xMnems',objVarMnems);
objVarWeights = [1,1,1];
H = size(baselineForecast.Forecast.modelVariables,2);
betaVec = optBeta.^(0:H-1);
objSimBase = baselineForecast.Forecast.modelVariables(objVarInds,1:H);
objSimOC = xSimul(objVarInds,1:H);
objSimOD = xSimulOD(objVarInds,1:H);
lossBase = betaVec.*(objVarWeights*(objSimBase.^2));
lossOC = betaVec.*(objVarWeights*(objSimOC.^2));
lossOD = betaVec.*(objVarWeights*(objSimOD.^2));
sumLossBase = cumsum(lossBase)/H;
sumLossOC = cumsum(lossOC)/H;
sumLossOD = cumsum(lossOD)/H;
disp('Losses:');
disp(['Baseline: ',num2str(sqrt(sumLossBase(end)))]);
disp(['Commitment: ',num2str(sqrt(sumLossOC(end)))]);
disp(['Discretion: ',num2str(sqrt(sumLossOD(end)))]);
