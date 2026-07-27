function [ data2 ] = standard( data )
%STANDARD Summary of this function goes here


   %local data2,datajm,dataj,i,j,datajst;
   data2=data;
   j=1;
   while j<=size(data,2)              % Loop obtains the following for each observation:
      dataj=data(:,j);              % Jeth column extraction
      dataj=dataj(dataj~=99999);    % Select valid data only 
      datajm=mean(dataj,1);         % Obtains mean
      datajst=std(dataj,1);         % Obtains standard deviation
      i=1;                      
      while i<=size(data,1)         %l oop that standardizes each column of data by 
                                    % subtracting the mean and deviding by standard deviation
         if data(i,j) ~= 99999
            data2(i,j)=(data(i,j)-datajm)/datajst; 
         end
         i=i+1;
      end
   j=j+1;                           % Iteration

   end
end


