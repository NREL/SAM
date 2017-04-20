C     Last change:  S     7 Mar 97    3:21 pm
      Subroutine SamOut(IRep)
C     INCLUDE 'KILLFILE.INC'                                            GDW-96  
      USE KILLFILE                      
c
c     This routine writes out the sample vector to Unit 1
c
C     INCLUDE 'PARMS.INC'                                               GDW-96  
      USE PARMS                         
C     INCLUDE 'CPARAM.INC'                                              GDW-96  
      USE CPARAM                        
C     INCLUDE 'CSAMP.INC'                                               GDW-96  
      USE CSAMP                         
      Character*(NamLen+1) OUTNAM
      Character*80 Line
c
c     -- Statement Function
c
      LOC(I,J)=(J-1)*N+I
c
c     -- If this is not the first replication or the named output format
c     -- has not been selected, then only write out the observations
c     -- and return.
c
      If (IRep /= 1  .OR.  NamOut /= 1) Then
c
c        -- Write the actual sample observations to the
c        -- file on unit 1 and return
c
         IF (I1Col == 0) THEN
C
C           -- Use the standard output format
            Do I=1, N
               Write(1,*)I,NV,(X(LOC(I,J)),J=1,NV)
            End Do
c
         ELSE
C
C           -- Use a single column output format
            Do I=1, N
               Write(1,*) I
               Write(1,*) NV
               DO J=1, NV
                  Write(1,*) X(LOC(I,J))
               END DO
            End Do
C
         END IF
c
         Return
C
      End If
c
c     -- Otherwise, the new output format has been selected and we need
c     -- to write out a header block.
c
c     -- Write comment lines for a header for the point
c     -- estimate section of the file.  These will tell the
c     -- user whether the point values are means or medians.
c
      Write (1,801)
      If (IPtVal == 0) Then
         Write (1,806)
      Else If (IPtVal == 1) Then
         Write (1,802)
      Else
         Write (1,803)
      End If
c
c     -- Now loop over variable names and
c     -- print them out with point values
c
      i = 0
      Do While ( IVarNm(i+1) /= 0 )
         i = i + 1
c
c        -- Print them out in the order encountered in the input file
c
c        -- If it is a "Same As" distribution, then do nothing and cycle
         If ( IVarNm(i) < 0  .AND.  IVarNm(i) /= -9999999 ) Cycle
c
c        -- Otherwise, calculate the point value to print out
c
         If ( IVarNm(i) == -9999999 ) Then
c           -- If it is a constant, just print it out.
            Value = PValue(i)
c
         Else If (IPtVal == 0) Then
c           -- print out the optional point values
            Value = PValue(i)
c
         Else If (IPtVal == 1) Then
c
c           -- calculate the mean
            iFirst = Loc(1,IVarNm(i))
            iLast = Loc(N,IVarNm(i))
            Value = 0.0
            Do j=iFirst, iLast
               Value = Value + X(j)
            End Do
            Value = Value / N
c
         Else
c
c           -- calculate the median - Note that XSave is already sorted
            iFirst = Loc(1,IVarNm(i))
            iLast = Loc(N,IVarNm(i))
            i2 = iFirst + (N/2)
            If ( 2 * ((iLast-iFirst) / 2) == (iLast-iFirst) ) Then
c              -- Even number of samples
               i1 = i2 - 1
               Value = 0.5 * ( XSave(i1) + XSave(i2) )
            Else
c              -- Odd number of samples
               Value = XSave(i2)
            End If
c
         End If
c
c        -- Check for other distributions that are the same as this one
         Line = List(i)
         NextP = 21
         j = 0
         Do While ( IVarNm(j+1) /= 0 )
            j = j + 1
            If ( IVarNm(j) == -i ) Then
               If ( NextP > 50 ) Then
                  Write (1,807) Line(1:NextP)
                  Line = ' '
                  NextP = 4
               End If
               Line(NextP:) = List(j)
               NextP = NextP + 17
            End If
         End Do
c
c        -- Write out the variable name and its point value
         Write (1,804) Line(1:NextP), Value
c
c        -- process the next variable
      End Do
c
c     -- Write one more comment line into the file as a spacer
      Write (1,805)
c
c     === Now print out the uncertainty analysis header block ===
c
c     -- Write out header block -    @UNCERTAINTY
c     --    number of observations - @OBSERVATIONS #
c     --    number of variables    - @VARIABLES #
c     --    followed by the list variable names in the order in
c     --    which their data will be encountered in the sampled data.
c
      Write (1,901)
      Write (1,902) N
      Write (1,903) NV
c
c     -- Now print out the list of variable names followed by :'s
c
      Do i=1, NV
c        -- Determine which name corresponds to this variable
         jHold = 0
         j = 0
         Do While ( IVarNm(j+1) /= 0 )
            j = j + 1
            If (IVarNm(j) == i) Then
               jHold = j
               Exit
            End If
         End Do
c        -- append a : to the name and print it
         OutNam = List(jHold)
         j = Index(OutNam,' ')
         OutNam(j:j) = ':'
c
c        -- Check for other distributions that are the same as this one
         Line = OutNam
         NextP = 19
         j = 0
         Do While ( IVarNm(j+1) /= 0 )
            j = j + 1
            If ( IVarNm(j) == -jHold ) Then
               If ( NextP > 65 ) Then
                  Write (1,807) Line(1:NextP)
                  Line = ' '
                  NextP = 19
               End If
               Line(NextP:) = List(j)
               NextP = NextP + 17
            End If
         End Do
c
c        -- Write out the last line of variable name(s)
         Write (1,808) Line(1:NextP)
c
      End Do
c
c     -- Write the keyword @SAMPLEDATA to start the sampled data block.
c
      Write (1,905)
c
c     --- Write the actual sample observations to the file
      IF (I1Col == 0) THEN
C
C        -- Use the standard output format
         Do I=1, N
            Write(1,*)I,NV,(X(LOC(I,J)),J=1,NV)
         End Do
c
      ELSE
C
C        -- Use a single column output format
         Do I=1, N
            Write(1,*) I
            Write(1,*) NV
            DO J=1, NV
               Write(1,*) X(LOC(I,J))
            END DO
         End Do
C
      END IF
c
      Return
c
  801 Format('$ Point Values for the distributions follow:',
     1     /,'$')
  802 Format('$ All point values represent mean values that',
     1     /,'$ were calculated from the actual LHS sample.',
     2     /,'$')
  803 Format('$ Median point values were determined from the',
     1     /,'$ actual LHS sample and printed below.',
     2     /,'$')
  804 Format(4x,A,1PE14.7)
  805 Format('$')
  806 Format('$ All point values represent the optional point ',
     1     /,'$ values that were found in the input file.',
     2     /,'$')
  807 Format(4x,A,'#')
  808 Format(4x,A)
c
  901 Format('@UNCERTAINTY')
  902 Format(2x,'@OBSERVATIONS ',I6)
  903 Format(2x,'@VARIABLES ',I6)
  905 Format('@SAMPLEDATA')
c
      End
