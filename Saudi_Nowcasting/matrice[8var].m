function [Rs,Qs,Hs,Fs]=matrices(z)
global vfq vector;
%---------------------------------------------------------------
% Kalman matrices — 8 variables, pnk=27
% GDP (q), reserves, exports, imports, brent, TASI, BOP (q), POS sales
%---------------------------------------------------------------
Rs = ones(1,8);

% GDP row: MM aggregation over factor lags (cols 1-5) and own idio lags (cols 6-10)
h01 = [vector'*z(1) vector' zeros(1,17)];  % 5+5+17=27

% Monthly variables: loading on factor (col 1), idio at respective position
h2 = [[1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0];   % reserves  idio col 11
      [0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0];   % exports   idio col 13
      [0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0];   % imports   idio col 15
      [0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0];   % brent     idio col 17
      [0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0]];  % TASI      idio col 19
% 5x17 block (cols 11-27)
Hs = [z(2:6) zeros(5,9) h2];   % 5x(1+9+17)=5x27
Hs = [h01; Hs];                 % 6x27

% BOP row: MM aggregation over factor lags (cols 1-5) and own idio lags (cols 21-25)
h_bop = [vector'*z(7) zeros(1,15) vector' zeros(1,2)];  % 5+15+5+2=27
Hs = [Hs; h_bop];              % 7x27

% POS sales row: monthly, loading on factor (col 1), idio at col 26
h_pos = [z(8) zeros(1,24) 1 0];  % 1+24+1+1=27
Hs = [Hs; h_pos];              % 8x27

z0 = z(9:10);    % factor AR(2)
z1 = z(11:12);   % GDP idio AR(2)
z2 = z(13:14);   % reserves
z3 = z(15:16);   % exports
z4 = z(17:18);   % imports
z5 = z(19:20);   % brent
z6 = z(21:22);   % TASI
z7 = z(23:24);   % BOP portfolio
z8 = z(25:26);   % POS sales

% Factor block (rows 1-5): AR(2) companion
f1   = [z0'               zeros(1,25)];
f1a  = [eye(4)            zeros(4,23)];

% GDP idio block (rows 6-10): AR(2) companion, 5 lags
f2   = [zeros(1,5)   z1'  zeros(1,20)];
f2a  = [zeros(4,5)   eye(4) zeros(4,18)];

% Monthly idio blocks (2 rows each): AR(2) companion
f3   = [zeros(1,10)  z2'  zeros(1,15)];
f3a  = [zeros(1,10)  1    zeros(1,16)];
f4   = [zeros(1,12)  z3'  zeros(1,13)];
f4a  = [zeros(1,12)  1    zeros(1,14)];
f5   = [zeros(1,14)  z4'  zeros(1,11)];
f5a  = [zeros(1,14)  1    zeros(1,12)];
f6   = [zeros(1,16)  z5'  zeros(1,9)];
f6a  = [zeros(1,16)  1    zeros(1,10)];
f7   = [zeros(1,18)  z6'  zeros(1,7)];
f7a  = [zeros(1,18)  1    zeros(1,8)];

% BOP idio block (rows 21-25): AR(2) companion, 5 lags
f8   = [zeros(1,20)  z7'  zeros(1,5)];
f8a  = [zeros(4,20)  eye(4) zeros(4,3)];

% POS idio block (rows 26-27): AR(2) companion
f9   = [zeros(1,25)  z8'          ];
f9a  = [zeros(1,25)  1    zeros(1,1)];

Fs = [f1;f1a;f2;f2a;f3;f3a;f4;f4a;f5;f5a;f6;f6a;f7;f7a;f8;f8a;f9;f9a];  % 27x27

qvec = [vfq 0 0 0 0 z(27) 0 0 0 0 z(28) 0 z(29) 0 z(30) 0 z(31) 0 z(32) 0 z(33) 0 0 0 0 z(34) 0];
qvec = (qvec).^2;
Qs   = diag(qvec);
end