% Nowcasting / Forecasting Homework
% Forecasting Saudi Arabia GDP — 8 variables (incl. BOP portfolio + POS sales)
% Based on Stock and Watson 1991
clear
clc
cd('/Users/aliaydinkara/Github/Saudi_Arabia_GDP')

global yv filter n vfq capt pnk vector index ny H filterptt;

va=1; vfq=1; pphi=2; nk=pphi+1;
vector=[(1/3);(2/3);1;(2/3);(1/3)];

%% Load data — 8 variables, Jan 2015 to Jun 2026 (B14:I151)
% Cols: GDP | reserves | PMI | imports | brent | POS sales | TASI | BOP | 
indica = xlsread('Data_Final.xlsx','Data_Non-oil','B14:I151');

% Convert Brent (col 5) from levels to log-differences
brent_lev   = indica(:,5);
brent_gr    = [NaN; diff(log(brent_lev))];
indica(:,5) = brent_gr;

% Convert sales (col 6) from levels to log-differences
pos_lev     = indica(:,6);
pos_gr      = [NaN; diff(log(pos_lev))];
indica(:,6) = pos_gr;

% Convert TASI (col 7) from levels to log-differences
tasi_lev    = indica(:,7);
tasi_gr     = [NaN; diff(log(tasi_lev))];
indica(:,7) = tasi_gr;

% BOP portfolio (col 8): already in decimal growth rates, quarterly (NaN off-months)

indica_raw = indica;  % save after transformations
indica(isnan(indica)) = 99999;

n = size(indica,2);  % = 8

%% Mean and std (observed values only)
gg = indica(indica(:,1)~=99999,1); megdp=mean(gg);  stdgdp=std(gg);
gg = indica(indica(:,2)~=99999,2); meind1=mean(gg); stdind1=std(gg);  % reserves
gg = indica(indica(:,3)~=99999,3); meind2=mean(gg); stdind2=std(gg);  % PMI
gg = indica(indica(:,4)~=99999,4); meind3=mean(gg); stdind3=std(gg);  % imports
gg = indica(indica(:,5)~=99999,5); meind4=mean(gg); stdind4=std(gg);  % brent
gg = indica(indica(:,6)~=99999,6); meind5=mean(gg); stdind5=std(gg);  % sales
gg = indica(indica(:,7)~=99999,7); meind6=mean(gg); stdind6=std(gg);  % bop_portfolio
gg = indica(indica(:,8)~=99999,8); meind7=mean(gg); stdind7=std(gg);  % TASI

%% Standardize and fill
indica  = standard(indica);
indica2 = relleno(indica,1);
y  = indica2;
ny = size(y,2);
capt = size(y,1);
pnk  = 27;  % 5 factor + 5 GDP idio + 5x2 monthly idio + 5 BOP idio + 2 POS idio

yv    = y;
index = (indica~=99999);

%% Starting values: 8 loadings + 2 factor AR + 16 idio AR + 8 variances = 34
startval=[
% Loadings z(1)-z(8)
0.267    % z(1)  GDP
0.497    % z(2)  reserves
0.604    % z(3)  PMI
0.430    % z(4)  imports
0.381    % z(5)  brent_gr
0.400    % z(6)  pos_sales
0.300    % z(7)  bop_portfolio
0.300    % z(8)  pos_sales (neutral start)

% Factor AR(2): z(9)-z(10)
0.423    % z(9)  AR1
0.255    % z(10) AR2

% Idio AR(2) x8: z(11)-z(26)
0.694;  -0.576    % z(11:12) GDP
0.197;   0.521    % z(13:14) PMI
-0.196; -0.159    % z(15:16) exports
-0.423; -0.222    % z(17:18) imports
-0.156; -0.003    % z(19:20) brent_gr
0.100;   0.100    % z(21:22) sales
0.100;   0.100    % z(23:24) bop_portfolio
0.100;   0.100    % z(25:26) tasi_gr

% Variances z(27)-z(34)
0.902219485    % z(27) GDP
0.860813569    % z(28) reserves
0.934344690    % z(29) credit
0.900555384    % z(30) imports
0.912688337    % z(31) brent_gr
0.900          % z(32) sales
0.900          % z(33) bop_portfolio
0.900          % z(34) tasi_gr
];

nth = length(startval);
filter = zeros(capt,pnk);

%% MLE
options = optimset('PlotFcns',@optimplotx,'Display','iter','TolFun',1e-8,...
                   'MaxFunEvals',20000,'MaxIter',5000);
[x,ff,EXITFLAG,OUTPUT,GRAD,HESSIAN] = fminunc(@ofn,startval,options);

%% Hessian and t-stats
cramerrao = inv(HESSIAN);
std_err   = sqrt(diag(cramerrao));
results   = [x std_err x./std_err];
fprintf('\n%s\n', repmat('-',1,60))
fprintf('%-20s %10s %10s %10s\n','Parameter','Estimate','Std Err','t-stat')
fprintf('%s\n', repmat('-',1,60))
varnames = {'GDP load','reserves load','PMI load','imports load',...
            'Brent load','sales load','BOP portf load','TASI load',...
            'factor AR1','factor AR2'};
for i=1:10
    fprintf('%-20s %10.4f %10.4f %10.4f\n', varnames{i}, results(i,1), results(i,2), results(i,3));
end
fprintf('%s\n', repmat('-',1,60))

%% Recover predicted series (de-standardized)
forecast = zeros(8,size(filter,1));
for i=1:size(filter,1)
    forecast(:,i) = H*filter(i,:)';
end
forecast2 = forecast';
gdp  = forecast2(:,1)*stdgdp  + megdp;
ind1 = forecast2(:,2)*stdind1 + meind1;
ind2 = forecast2(:,3)*stdind2 + meind2;
ind3 = forecast2(:,4)*stdind3 + meind3;
ind4 = forecast2(:,5)*stdind4 + meind4;
ind5 = forecast2(:,6)*stdind5 + meind5;
ind6 = forecast2(:,7)*stdind6 + meind6;
ind7 = forecast2(:,8)*stdind7 + meind7;

%% Factor vs GDP plot
factor = filter(:,1);
factor_q = 1/3*factor(5:end) + 2/3*factor(4:end-1) + factor(3:end-2) + ...
    2/3*factor(2:end-3) + 1/3*factor(1:end-4);

gdp_q         = indica_raw(:,1);
gdp_q_aligned = gdp_q(5:end);
dates         = datetime(2015,1,1) + calmonths(0:capt-1);
dates_q       = dates(5:end);

figure('Position',[100 100 900 450]);
yyaxis left
plot(dates_q, factor_q, 'b-', 'LineWidth', 1.5);
ylabel('Quarterly aggregated factor', 'Color','b');
yyaxis right
plot(dates_q, gdp_q_aligned, 'ro', 'MarkerSize', 7, 'MarkerFaceColor', 'r');
ylabel('Quarterly GDP growth (%)', 'Color','r');
hold on; yyaxis left
covid_start = datetime(2020,3,1); covid_end = datetime(2020,6,1);
ylim_curr = ylim;
patch([covid_start covid_end covid_end covid_start], ...
    [ylim_curr(1) ylim_curr(1) ylim_curr(2) ylim_curr(2)], ...
    [0.8 0.8 0.8], 'FaceAlpha', 0.3, 'EdgeColor','none');
plot(dates_q, factor_q, 'b-', 'LineWidth', 1.5);
title('Extension: Estimated Factor vs Quarterly Non-Oil GDP Growth');
xlabel('Date'); grid on;
legend('Factor (quarterly aggregated)','GDP growth','COVID period','Location','SouthWest');

%% Correlation: factor vs actual GDP
gdp_q_only    = gdp_q_aligned(~isnan(gdp_q_aligned));
factor_at_gdp = factor_q(~isnan(gdp_q_aligned));
corr_value = corr(factor_at_gdp, gdp_q_only);
fprintf('Correlation between factor and GDP (quarterly): %.4f\n', corr_value);

%% Internal diagnostic (gdp vs gdp2)
H2 = H;
H2(1,6:10) = 0;
forecast_d = zeros(8,size(filter,1));
for i=1:size(filter,1)
    forecast_d(:,i) = H2*filter(i,:)';
end
forecast2_d = forecast_d';
gdp2 = forecast2_d(:,1)*stdgdp + megdp;
caca = [gdp gdp2];
idx  = 1:3:size(caca,1);
sub  = caca(idx,:);
fprintf('Internal diagnostic correlation (gdp vs gdp2): %.4f\n', corr(sub(:,1),sub(:,2)));

%% GDP Forecast — next two quarters
[R, Q, H, F] = matrices(x);

% Extend factor 5 months ahead using state transition
kk  = filter(end,:)';
kk1 = F*kk;
kk2 = F*kk1;
kk3 = F*kk2;
kk4 = F*kk3;
kk5 = F*kk4;

% Build extended factor series
factor2 = [factor; kk1(1); kk2(1); kk3(1); kk4(1); kk5(1)];

% Mariano-Murasawa quarterly aggregation on extended series
factorq2 = 1/3*factor2(5:end) + 2/3*factor2(4:end-1) + factor2(3:end-2) + ...
           2/3*factor2(2:end-3) + 1/3*factor2(1:end-4);

% Subsample at quarterly frequency
i = 1; factgdp2 = [];
while i <= length(factorq2)
    factgdp2 = [factgdp2; factorq2(i)];
    i = i + 3;
end

% Subsample historical factor at quarterly frequency for OLS
i = 1; factgdp = [];
while i <= length(factor_q)
    factgdp = [factgdp; factor_q(i)];
    i = i + 3;
end

% Get observed GDP values
gdp_obs = indica_raw(:,1);
gdp_obs = gdp_obs(~isnan(gdp_obs));

% Align dimensions for OLS
m = min(length(gdp_obs), length(factgdp));
y_ols = gdp_obs(1:m);
fg    = factgdp(1:m);

% OLS regression: GDP = a + b*factor
x_reg = [ones(m,1) fg];
b = (x_reg'*x_reg)\(x_reg'*y_ols);

% Forecast Q+1 and Q+2
yhat1 = b(1) + b(2)*factgdp2(end-1);
yhat2 = b(1) + b(2)*factgdp2(end);

fprintf('OLS: intercept=%.4f, slope=%.4f\n', b(1), b(2));
fprintf('Forecast Q+1 (GDP growth): %.4f (%.2f%%)\n', yhat1, yhat1*100);
fprintf('Forecast Q+2 (GDP growth): %.4f (%.2f%%)\n', yhat2, yhat2*100);