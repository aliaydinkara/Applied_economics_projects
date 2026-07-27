function [data2]=relleno(data,va)
   rd=size(data,1);
   cd=size(data,2);
   data2=va*randn(rd,cd);
   j=1;
   while j<=cd
      i=1;
      while i<=rd
         if data(i,j) ~= 99999;
            data2(i,j)=data(i,j);       % put the data back if it is not 99999
         end
         i=i+1;
      end;   
      j=j+1;
           
   end;

end

   
