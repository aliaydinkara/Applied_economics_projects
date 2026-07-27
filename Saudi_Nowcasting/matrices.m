function [Rs,Qs,Hs,Fs]=matrices(z)
global vfq vector;
%---------------------------------------------------------------
% Kalman matrices — 6 variables, pnk=20
% GDP (q), reserves, exports, imports, brent, POS sales
% All monthly except GDP (quarterly, Mariano-Murasawa)
%---------------------------------------------------------------
Rs = ones(1,6);

% GDP row: MM aggregation over factor lags (cols 1-5) and own idio lags (cols 6-10)
h01 = [vector'*z(1) vector' zeros(1,10)];  % 5+5+10=20

% Monthly variables: loading on factor (col 1), idio at respective position
h2 = [[1 0 0 0 0 0 0 0 0 0];   % reserves  idio col 11
      [0 0 1 0 0 0 0 0 0 0];   % exports   idio col 13
      [0 0 0 0 1 0 0 0 0 0];   % imports   idio col 15
      [0 0 0 0 0 0 1 0 0 0];   % brent     idio col 17
      [0 0 0 0 0 0 0 0 1 0]];  % POS sales idio col 19
% 5x10 block (cols 11-20)
Hs = [z(2:6) zeros(5,9) h2];   % 5x(1+9+10)=5x20
Hs = [h01; Hs];                 % 6x20

z0 = z(7:8);     % factor AR(2)
z1 = z(9:10);    % GDP idio AR(2)
z2 = z(11:12);   % reserves
z3 = z(13:14);   % exports
z4 = z(15:16);   % imports
z5 = z(17:18);   % brent
z6 = z(19:20);   % POS sales

% Factor block (rows 1-5): AR(2) companion
f1   = [z0'               zeros(1,18)];
f1a  = [eye(4)            zeros(4,16)];

% GDP idio block (rows 6-10): AR(2) companion, 5 lags
f2   = [zeros(1,5)   z1'  zeros(1,13)];
f2a  = [zeros(4,5)   eye(4) zeros(4,11)];

% Monthly idio blocks (2 rows each): AR(2) companion
f3   = [zeros(1,10)  z2'  zeros(1,8)];
f3a  = [zeros(1,10)  1    zeros(1,9)];
f4   = [zeros(1,12)  z3'  zeros(1,6)];
f4a  = [zeros(1,12)  1    zeros(1,7)];
f5   = [zeros(1,14)  z4'  zeros(1,4)];
f5a  = [zeros(1,14)  1    zeros(1,5)];
f6   = [zeros(1,16)  z5'  zeros(1,2)];
f6a  = [zeros(1,16)  1    zeros(1,3)];
f7   = [zeros(1,18)  z6'          ];
f7a  = [zeros(1,18)  1    zeros(1,1)];

Fs = [f1;f1a;f2;f2a;f3;f3a;f4;f4a;f5;f5a;f6;f6a;f7;f7a];  % 20x20

qvec = [vfq 0 0 0 0 z(21) 0 0 0 0 z(22) 0 z(23) 0 z(24) 0 z(25) 0 z(26) 0];
qvec = (qvec).^2;
Qs   = diag(qvec);
end