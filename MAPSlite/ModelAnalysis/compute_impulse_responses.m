function [irCellArray,xir,Yir] = compute_impulse_responses(...
    Model,H,S,zImpulseMnems)
% This model analysis function computes impulse responses for an LSS model.
% It allows for impulses to unanticipated shocks (standard) or anticipated
% shocks with a choice over the horizon of anticipation.
% 
% INPUTS:
%   -> Model: LSS model structure
%   -> H: horizon/number of periods of impulses
%   -> S (optional): periods ahead which the shock is anticipated
%   -> zImpulseMnems (optional): vector cell string array of shock
%      mnemonics for impulses to compute or, optionally, a single shock
%      mnemonic string if the responses to only one shock are required
%
% OUTPUTS:
%   -> irCellArray: nResponses*3 cell array of variable mnemonic, shock 
%      mnemonics, response vector of dimension 1*H triplets
%   -> xir: nx*H*nz matrix of model variable responses
%   -> Yir (model dependent): nY*H*nz matrix of model observable responses
%
% DETAILS:
%   -> This impulse response function computes impulse responses of an LSS 
%      model's variables and observables to either unanticipated or 
%      anticipated shocks. 
%   -> The default (if the optional 3rd input is not passed in) is for 
%      unanticipated impulse responses, which is equivalent to S=0 - i.e. 
%      compute_impulse_responses(Model,H,0) will deliver the same result as
%      compute_impulse_responses(Model,H)
%   -> The set of impulse responses computed and returned can be controlled
%      using the optional 4th input argument to reduce the set to a chosen
%      sub-set of the shocks.
%   -> The outputs are a three-column cell array of variable mnemonic - 
%      shock mnemonic - row response vector triplets and matrices of 
%      nVars*H*nShocks responses for the model variables and observables 
%      (if the model being used contains model observables). If only a sub-
%      set of the impulse responses are computed (i.e. if the optional 4th 
%      input argument is used), then the matrix outputs are ordered in the
%      3rd dimension consistenly with the ordering of the shock mnemonics
%      input.
%   -> The inputs and outputs specified in the call to this function must
%      be consistent with the model being used. It is invalid to request
%      anticipated impulse responses (S>0) if the model is not forward 
%      looking and it is invalid to request a matrix of model observable
%      responses if the model does not contain measurement equations.
%
% NOTES:
%   -> Please see the MAPS user guide for more information on model
%      analysis functionality in MAPS.
%
% This version: 25/01/2013
% Author(s): Francesca Monti & Matt Waldron

%% CHECK INPUTS
if nargin < 2
    errId = ['MAPS:',mfilename,':BadNargin'];
    generate_and_throw_MAPS_exception(errId);
elseif ~isstruct(Model)
    errId = ['MAPS:',mfilename,':BadInput1'];
    generate_and_throw_MAPS_exception(errId);
elseif ~is_positive_real_integer(H)
    errId = ['MAPS:',mfilename,':BadInput2'];
    generate_and_throw_MAPS_exception(errId);
elseif nargin>2 && ~is_non_negative_real_integer(S)
    errId = ['MAPS:',mfilename,':BadInput3'];
    generate_and_throw_MAPS_exception(errId);
elseif nargin>3 && ~is_string_or_vector_cell_string_array(zImpulseMnems)
    errId = ['MAPS:',mfilename,':BadInput4'];
    generate_and_throw_MAPS_exception(errId);    
end

%% HANDLE OPTIONAL ANTICIPATION HORIZON INPUT
% If the anticipation horizon input were not provided, then set the
% anticipation horizon to 0 (which is the same as unanticipated impulses).
if nargin < 3
    S = 0;
end

%% CHECK MODEL AGAINST COMBINATION OF INPUTS & OUTPUTS
modelIsLinearStateSpace = unpack_model(Model,{'modelIsLinearStateSpace'});
if ~modelIsLinearStateSpace
    errId = ['MAPS:',mfilename,':BadModelClass'];
    generate_and_throw_MAPS_exception(errId);
end
[modelIsForwardLooking,modelHasMeasurementEqs] = unpack_model(...
    Model,{'modelIsForwardLooking','modelHasMeasurementEqs'});
if S>0 && ~modelIsForwardLooking
    errId = ['MAPS:',mfilename,':InvalidAnticipationInstruction'];
    generate_and_throw_MAPS_exception(errId);
elseif nargout>2 && ~modelHasMeasurementEqs
    errId = ['MAPS:',mfilename,':InvalidModelObsOutputRequest'];
    generate_and_throw_MAPS_exception(errId);
end

%% SET FLAG FOR ANTICIPATED RESPONSES
if S > 0
    responsesAreAnticipated = true;
else
    responsesAreAnticipated = false;
end

%% UNPACK MODEL & COMPUTE DIMENSIONS OF VARIABLES & SHOCKS
% Unpack all components of the model required for what follows, computing
% the dimensions of model variables, shocks and, if applicable, model
% observables.
[B,PHI,F,xMnems,zMnems] = unpack_model(...
    Model,{'B','PHI','F','xMnems','zMnems'});
[nx,nz] = size(PHI);
if modelHasMeasurementEqs
    [G,Ymnems] = unpack_model(Model,{'G','Ymnems'});
    nY = size(G,1);
end

%% HANDLE OPTIONAL CHOICE OF SHOCKS INPUT
% If the choice of shocks input argument was not passed in, then this
% function computes the impulse responses to all shocks, 1 to nz. If this
% optional input were pased in, then attempt to compute the associated
% shock indices, catching and handling any errors as appropriate (which
% will arise if any of the mnemonics passed in do not exist in the model
% set or if there are repetitions).
if nargin < 4
    zImpulseInds = (1:nz)';
else
    zImpulseMnems = ...
        convert_string_or_vector_string_array_to_column_array(...
        zImpulseMnems);
    try
        zImpulseInds = lookup_model_index_numbers(zMnems,zImpulseMnems);
    catch LookupE
        errId = ['MAPS:',mfilename,':InvalidShocksToImpulseInstruction'];
        generate_MAPS_exception_add_cause_and_throw(LookupE,errId);
    end
end

%% INITIALISE OUTPUT
% Note that the responses for model observables can only be computed if the
% model being used has measurement equations and that that also determines
% the size of the cell array output.
nzImpulses = size(zImpulseInds,1);
xir = zeros(nx,H,nzImpulses);
nVars = nx;
if modelHasMeasurementEqs
    Yir = zeros(nY,H,nzImpulses);
    nVars = nVars+nY;
end
irCellArray = cell(nVars*nzImpulses,3);

%% COMPUTE IMPULSE RESPONSES
% Loop over the shocks for which to compute the impulse responses. For each
% one (taking care to select the correct one using ths indices computed
% above): compute the responses of the model variables either to an
% anticipated shock or to an unanticipated shock and store the result in
% the matrix and cell array; if applicable, compute the associated model
% observable responses and store the result in a matrix and in the cell
% array. Note that the logic for packing the cell array is contained in a
% sub-function below and that this also includes logic (dictated by the
% final input passed in below) for where to put the model variable and
% model observable responses in that cell array.
for izImpulse = 1:nzImpulses
    izImpulseInd = zImpulseInds(izImpulse);
    izImpulseMnem = zMnems{izImpulseInd};
    if responsesAreAnticipated
        ixir = compute_impulse_responses_to_an_anticipated_shock(...
            B,PHI,F,izImpulseInd,H,S,nx);
    else
        ixir = compute_impulse_responses_to_an_unanticipated_shock(...
            B,PHI,izImpulseInd,H,nx);
    end
    xir(:,:,izImpulse) = ixir;
    irCellArray = pack_impulse_response_cell_array(irCellArray,...
        xMnems,izImpulseMnem,ixir,nx,H,izImpulse,0);
    if modelHasMeasurementEqs
        iYir = compute_model_observable_impulse_responses_to_shock(ixir,G);
        Yir(:,:,izImpulse) = iYir;
        irCellArray = pack_impulse_response_cell_array(irCellArray,...
            Ymnems,izImpulseMnem,iYir,nY,H,izImpulse,nx*nzImpulses);
    end
end

end

%% FUNCTION TO COMPUTE IMPULSE RESPONSES TO AN UNANTICIPATED SHOCK
function xir = compute_impulse_responses_to_an_unanticipated_shock(...
    B,PHI,zImpulseInd,H,nx)
% This function contains the logic for responses to an unanticipated shock.
% 
% INPUTS:
%   -> B: state transition matrix
%   -> PHI: shock loadings matrix
%   -> zImpulseInd: index of shock to impulse
%   -> H: horizon over which to compute the responses
%   -> nx: number of model variables
%
% OUTPUTS:
%   -> xir: nx*H matrix of responses to the unanticipated shock

%% INITIALISE
xir = NaN*ones(nx,H);

%% COMPUTE IMPACT OF SHOCKS
% At time zero, the model variables are at steady state (equal zero) and
% the size of the shocks is standardised to 1, so the impact of the shock
% in period 1 is just the relevant column of the shock loadings matrix.
xir(:,1) = PHI(:,zImpulseInd);

%% COMPUTE TRANSITION
% After period 1, there are no more shocks so the transition is just
% determined by the state transition matrix.
for t = 2:H
    xir(:,t) = B*xir(:,t-1);
end

end

%% FUNCTION TO COMPUTE IMPULSE RESPONSES TO AN ANTICIPATED SHOCK
function xir = compute_impulse_responses_to_an_anticipated_shock(...
    B,PHI,F,zImpulseInd,H,S,nx)
% This function contains the logic for responses to an unanticipated shock.
% 
% INPUTS:
%   -> B: state transition matrix
%   -> PHI: shock loadings matrix
%   -> F: forward loadings matrix for anticipated shocks
%   -> zImpulseInd: index of shock to impulse
%   -> H: horizon over which to compute the responses
%   -> S: periods ahead which the shock is anticipated
%   -> nx: number of model variables
%
% OUTPUTS:
%   -> xir: nx*H matrix of responses to the unanticipated shock

%% INITIALISE
xir = NaN*ones(nx,H);

%% COMPUTE IMPACT OF SHOCKS
% At time zero, the model variables are at steady state (equal zero) and
% the size of the shocks is standardised to 1, so the impact of the shock
% in period 1 is just the relevant column of the shock loadings matrix 
% multiplied by the forward loadings matrix raised to the power of S.
xir(:,1) = F^S*PHI(:,zImpulseInd);

%% COMPUTE TRANSITION
% After period 1, the transition depends on whether the shock has occurred
% or not. If it has not, then the shock still has an impact, which is
% discounted to an extent that depends on how far ahead the shock is
% expected. Once the shock has occurred, the transition just depends on the
% state and the state transition matrix as in the unanticipated case.
for t = 2:H
    if t <= S+1
        xir(:,t) = B*xir(:,t-1)+F^(S+1-t)*PHI(:,zImpulseInd);
    else
        xir(:,t) = B*xir(:,t-1);
    end
end

end

%% FUNCTION TO COMPUTE MODEL OBSERVABLE IMPULSE RESPONSES TO A SHOCK
function Yir = compute_model_observable_impulse_responses_to_shock(xir,G)  
% This function contains the logic for responses to an unanticipated shock.
% 
% INPUTS:
%   -> xir: nx*H matrix of model variable responses to a particular shock
%   -> G: loadings on model variables in measurement equations
%
% OUTPUTS:
%   -> Yir: nY*H matrix of model observable responses to the same shock

%% COMPUTE MODEL OBSERVABLE RESPONSES FROM MODEL VARIABLES
% Note that the constants in the measurement equations are not included in
% the computation because, by definition of being impulses responses, we
% want deviations from steady state (or any abitrary baseline).
Yir = G*xir;  

end

%% FUNCTION TO PACK IMPULSE RESPONSES TO A SHOCK INTO IMPULSE CELL ARRAY
function irCellArray = pack_impulse_response_cell_array(irCellArray,...
    varMnems,zImpulseMnem,irMat,nVars,H,shockCounter,irCellIndBase)
% This helper contains the logic for packing the impulse response cell.
% 
% INPUTS:
%   -> irCellArray: three-column cell array of variable-shock-response
%      vector triplets
%   -> varMnems: either model variable or model observable mnemonics
%   -> zImpulseMnem: mnemonic of the shock used to compute the impulse
%   -> irMat: nVars*H matrix for the response of each variable to the shock
%   -> nVars: number of variables
%   -> H: horizon over which the responses were computed
%   -> shockCounter: number of shocks (including the current shock) for
%      which impulses have been computed
%   -> irCellIndBase: base index for the pack to account for the fact that
%      model observable responses are packed separately and must be
%      appended to the model variable responses already packed
%
% OUTPUTS:
%   -> irCellArray: updated three column cell array

%% COMPUTE THE START AND END INDICES
irCellIndStart = irCellIndBase+nVars*(shockCounter-1)+1;
irCellIndEnd = irCellIndBase+nVars*shockCounter;

%% PACK THE VARAIBLE MNEMONICS IN COLUMN 1    
irCellArray(irCellIndStart:irCellIndEnd,1) = varMnems;

%% PACK THE SHOCK MNEMONIC IN COLUMN 2
irCellArray(irCellIndStart:irCellIndEnd,2) = repmat(...
    {zImpulseMnem},[nVars 1]);

%% PACK THE NUMERIC RESPONSES AS INDIVIDUAL VECTORS IN COLUMN 3
irCellArray(irCellIndStart:irCellIndEnd,3) = mat2cell(...
    irMat,ones(1,nVars),H);
    
end