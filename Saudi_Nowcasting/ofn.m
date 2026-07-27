function [ fun ] = ofn( th )
%OFN Summary of this function goes here
    % Detailed explanation goes here
    % local like,beta00,P00,it,beta10,beta11,n10,fun,Hit,mit,Rit,
    % R,F10,Q,P10,P11,W; 
   
   global yv filter n vfq capt index pnk filterptt; % Specifying global variables
   global H;
   
   [R,Q,H,F]=matrices(th);                % Given th, we obtain matrices R, Q, H, F
   beta00=zeros(pnk,1);                   % Matrix (10x1)
   P00=eye(pnk);                          % Identity Matrix (10x10)
   like=zeros(capt,1);                    % Matrix (T-1x1)

   %% KALMAN FILTER
   
   it=1;                                  % kalman filter iterations
   while it<=capt;                        % While iteration number is smaller than or equal to number of observations
      
      % Set row of the H matrix to zero if it is not observed
      Hit = bsxfun(@times,index(it,:)',H);  
      % In contrast, set R to zero if it is observed
      Rit = diag(bsxfun(@times,(1-index(it,:)),R));   %Rit=diagrv(zeros(ny,ny),(1-index(it,:))'.*R); 

      if it == 4   %TEMP
        disp(index(it,:)) %TEMP
        disp(rank(Hit)) %TEMP
        disp(diag(Rit)') %TEMP
      end %TEMP

      beta10=F*beta00;                      % Prediction equations
      P10=F*P00*F'+Q;
      
      n10=yv(it,:)'-(Hit*beta10);           % Error forecast
      F10=Hit*P10*Hit'+Rit;
            

      like(it)=-0.5*(log(2*pi*abs(det(F10)))+(n10'/F10)*n10);    % Likelihood function
     
      K=P10*(Hit'/F10);                     % Kalman gain 
      
      beta11=beta10+K*n10;                  % Updating equations 
      filter(it,:)=beta11';
      P11=P10-K*Hit*P10;
      %filterptt(it,:,:)=P11;
          
      beta00=beta11;
      P00=P11;      

      it=it+1;                              %Iterating
   end
   
     fun=-(sum(like(15:end),1));                    % Sum of the likelihood
     
end

