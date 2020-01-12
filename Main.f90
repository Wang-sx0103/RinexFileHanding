Program Main
    
    Implicit none
    
    Integer :: status = 0                 !文件处理状态开关
    !导航电文的观测数据，以同一观测类型为一矩阵，每行代表一颗卫星在不同历元时的数据
    Character(len = 3) SatSNR(40,24)
    Real(Kind = 16) AF0(40,24)
    Real(Kind = 16) AF1(40,24)
    Real(Kind = 16) AF2(40,24)
    !第二行
    Real(Kind = 16) IODE(40,24)
    Real(Kind = 16) Crs(40,24)
    Real(Kind = 16) Deltan(40,24)
    Real(Kind = 16) M0(40,24)
    !第三行
    Real(Kind = 16) Cuc(40,24)
    Real(Kind = 16) e(40,24)
    Real(Kind = 16) Cus(40,24)
    Real(Kind = 16) Roota(40,24)
    !第四行
    Real(Kind = 16) TOE(40,24)
    Real(Kind = 16) Cic(40,24)
    Real(Kind = 16) Omega0(40,24)
    Real(Kind = 16) Cis(40,24)
    !第五行
    Real(Kind = 16) i0(40,24)
    Real(Kind = 16) Crc(40,24)
    Real(Kind = 16) Omega(40,24)
    Real(Kind = 16) OmegaDot(40,24)
    !第六行
    Real(Kind = 16) IDOT(40,24)
    Real(Kind = 16) Wn(40,24)
    !第七行
    Real(Kind = 16) Precise(40,24)
    Real(Kind = 16) Health(40,24)
    Real(Kind = 16) TGD(40,24)
    Real(Kind = 16) IODC(40,24)
    !第八行
    Real(Kind = 16) StartSec(40,24)
    Real(Kind = 16) TLong(40,24)
    
    !X、Y、Z坐标的存储
    Real(kind = 16):: X(40,2880)
    Real(kind = 16):: Y(40,2880)
    Real(kind = 16):: Z(40,2880)
    
    !高度角
    Real(Kind = 16) :: Ref_1_Rox(40,2880)
    Real(Kind = 16) :: Ref_1_Roy(40,2880)
    Real(Kind = 16) :: Ref_1_Roz(40,2880)
    Real(Kind = 16) :: Ref_1_R0(40,2880)
    Real(Kind = 16) :: Ref_1_elev(40,2880)
    
    !用户坐标(X,Y,Z)
    Real(kind = 16):: Ref_1_X
    Real(kind = 16):: Ref_1_Y
    Real(kind = 16):: Ref_1_Z
    
    !头文件
    ! Character(len = 60) IONOSPHERIC_CORR
    ! Character(len = 60) TIME_SYSTEM_CORR
    ! Character(len = 60) CORREND_OF_HEADER
    
    !!!过度数据
    Real(kind = 16) Interim
    Character (len = 60) Title_Detaile   !文件头1-60列内容
    Character (len = 20) Title           !文件头61-80列内容
    Character (len = 3)  SSN             !卫星系统及其编号
    Character (len = 80) one_row         !每行数据暂存处
    Character (len = 3) Flag             !用于控制行号增加的开关
    Character (Len = 500) Str            !输出卫星某一历元对应观测角
    Character (Len = 500) Str_First      !输出卫星名称的字符串
    Character (Len = 9) Interim_Str
    Character (Len = 3) Interim_First
    Character (len = 9) :: Empty_Str = "        0"
    !!!卫星位置、高度角计算过程
    Real(Kind = 16) T
    Real(Kind = 16) Deltt
    Real(Kind = 16) Sqrta
    Real(Kind = 16) a
    Real(Kind = 16) n0
    Real(Kind = 16) n
    Real(Kind = 16) tk
    Real(Kind = 16) mk
    Real(Kind = 16) ek1
    Real(Kind = 16) ek2
    Real(Kind = 16) Ek
    Real(Kind = 16) Sinfk
    Real(Kind = 16) Cosfk
    Real(Kind = 16) fk
    Real(Kind = 16) phik
    Real(Kind = 16) deltauk
    Real(Kind = 16) deltark
    Real(Kind = 16) deltaik
    Real(Kind = 16) uk
    Real(Kind = 16) rk
    Real(Kind = 16) ik
    Real(Kind = 16) xk
    Real(Kind = 16) yk
    Real(Kind = 16) Lk
    Real(Kind = 16) tX1
    Real(Kind = 16) tY1
    Real(Kind = 16) tZ1
    Real(Kind = 16) delt_z
    Real(Kind = 16) delt_x
    Real(Kind = 16) rx
    Real(Kind = 16) ry
    Real(Kind = 16) rz
    Real(Kind = 16) rr
    Real(Kind = 16) rr0
    Real(Kind = 16) theta
    Real(Kind = 16) Dot
    Real(Kind = 16) mag1
    Real(Kind = 16) mag2
    Integer :: Time = 1
    !用到的常数
    Real(Kind = 16),parameter :: GM = 3.986004E14
    Real(Kind = 16),parameter :: Ve = 7.2921151467E-5
    Real(Kind = 16),parameter :: pi=3.1415926535897932
    Real(Kind = 16),parameter :: mu=3.9860047000E14
    Real(Kind = 16),parameter :: vel_light= 299792458.0
    Real(Kind = 16),parameter :: earthrot = 7.2921151467E-5
    !计数器的界限
    Integer ,parameter:: limit1 = 8
    Integer ,parameter:: limit2 = 2880              !120乘以24内插卫星坐标个数
    Integer ,parameter:: limit3 = 24                !一天24小时
    Integer ,parameter:: limit4 = 40                !北斗卫星的个数
    !计数器
    Integer:: Counter = 1                           !控制AF0、AF1这类观测数据的行号
    Integer:: Counter1 = 0                          !控制AF0、AF1这类观测数据的列号
    Integer:: Counter2 = 1
    Integer:: Epoch = 1                             !120乘以24内插卫星坐标个数
    Integer:: Hour = 1                              !一天的界限
    Integer:: Sat_Num = 1                           !北斗卫星的个数
    !!!文件读写准备工作
    !Real(kind = 16) Interim
    integer ,parameter :: fileid1 = 10              !指令文件序号
    integer ,parameter :: fileid2 = 20              !N文件序号
    integer ,parameter :: fileid3 = 30              !O文件序号
    integer ,parameter :: fileid4 = 40              !高度角文件序号
    logical ::alive                                 !文件存在性检验
    character (len = 200) filename1                  !指令文件地址
    character (len = 200) filename2                  !导航文件地址
    character (len = 200) filename3                  !观测文件地址
    character (len = 200) filename4                  !高度角文件地址
    
    !观测文件的观测数据，只读3频卫星
    !PR: Pseudo Range,CP:Carrier Phase,Do:Doppler,SS:Signal Strength
    !1:B1/1561.098，2:B2/1207.14,B3/1268.52,每种频率下包含三种频道（I、Q、I+Q）,
    !但一个频率下仅有一种频道出现
    Real(Kind = 8) :: C_1_PR(40,2880)
    Real(Kind = 8) :: C_2_PR(40,2880)
    Real(Kind = 8) :: C_3_PR(40,2880)
    Real(Kind = 8) :: C_1_CP(40,2880)
    Real(Kind = 8) :: C_2_CP(40,2880)
    Real(Kind = 8) :: C_3_CP(40,2880)
    Real(Kind = 8) :: C_1_Do(40,2880)
    Real(Kind = 8) :: C_2_Do(40,2880)
    Real(Kind = 8) :: C_3_Do(40,2880)
    Real(Kind = 8) :: C_1_SS(40,2880)
    Real(Kind = 8) :: C_2_SS(40,2880)
    Real(Kind = 8) :: C_3_SS(40,2880)
    Character(len = 1) SatName
    Integer  TypeNum
    Integer  Interim_Int
    Character(len = 60)  ObsTypes     !观测类型的排列顺序
    Character(len = 322) DataRow      !观测文件的数据行 20*(14+2)+3 = 323,最多的情况为伽利略系统5分频率的观测信号
    
    
    !write(*,*)
    write(*,*) "指令文件规格见协议！"
    write(*,*) "输入指令文件地址："
    read(*,"(A200)") filename1
    
    inquire(file = trim(filename1),exist = alive)     !!!!查询文件的状态
    !!!判断文件是否存在
    If (.not. alive) then
        write(*,*) trim(filename1),"不存在!"
        read(*,*)
        stop
    End If
    
    !!!!!打开指令文件!
    open(unit = fileid1,file = filename1)
    !读取指令文件
    Write(*,*)
    write(*,*)"指令文件正在读取..."
    !!获取导航、观测、高度角文件的地址
    read(unit = fileid1,fmt = "(A200)",iostat = status) filename2
    read(unit = fileid1,fmt = "(A200)",iostat = status) filename3
    read(unit = fileid1,fmt = "(A200)",iostat = status) filename4
    
    !!判断指令文件是否处理读取完成
    If (status /= 0) Then
        write(*,*)
        write(*,*) "指令文件读取完成..."
        Close(fileid1)
    Else
        read(unit = fileid1,fmt = "(A80)",iostat = status) one_row
        read(one_row(1:13),"(F14.4)")  Ref_1_X
        read(unit = fileid1,fmt = "(A80)",iostat = status) one_row
        read(one_row(1:13),"(F14.4)")  Ref_1_Y
        read(unit = fileid1,fmt = "(A80)",iostat = status) one_row
        read(one_row(1:13),"(F14.4)")  Ref_1_Z
        Write(*,*)
        Write(*,*) "指令文件读取完成..."
        Close(fileid1)
    End If
        
    inquire(file = trim(filename2),exist = alive)     !!!!查询导航文件的状态
    !判断文件是否存在
    If (.not. alive) Then
        write(*,*)
        write(*,*) "导航文件不存在!"
        read(*,*)
        stop
    End If
    
    !打开导航文件
    Open(unit = fileid2,file = filename2)
    
    !!!处理导航文件的文件头
    Write(*,*)
    write(*,*)"导航文件正在处理..."
    Do While(.true.)
        read(unit = fileid2,fmt = "(A60,A20)") Title_Detaile,Title
        
        If (Trim(Title) == "IONOSPHERIC CORR") Then
            Cycle 
        Else If (Trim(Title) == "TIME SYSTEM CORR") Then
            Cycle 
        Else IF (Trim(Title) == "END OF HEADER") Then
            Exit 
        End If
    End Do
    
    !!!处理导航文件的数据部分
    SSN = "C01"
    Flag = "C01"
    Do While (.True.)
        !!按行读取数据
        read(unit = fileid2,fmt = "(A80)",iostat = status) one_row
        !!判断文件是否处理完
        If (status /= 0) Then
            Write(*,*)
            write(*,*) "导航文件处理完成..."
            exit
        End If
        SSN = one_row(1:3)
        If (one_row(1:1) == "C") Then
            Counter1 = Counter1 + 1
            Do Counter2 = 1 ,limit1,1
                If (Counter2 == 1) Then
                    If (Flag /= SSN) Then
                        Counter = Counter + 1
                        Counter1 = 1
                        Flag = SSN
                    Else
                        Flag = SSN
                    End If
                    SatSNR(Counter,Counter1) = one_row(1:3)
                    SSN = one_row(1:3)
                    read(one_row(24:42),"(D19.12)") Interim
                    AF0(Counter,Counter1) = Interim
                    read(one_row(43:61),"(D19.12)") Interim
                    AF1(Counter,Counter1) = Interim
                    read(one_row(62:80),"(D19.12)") Interim
                    AF2(Counter,Counter1) = Interim
                Else If (Counter2 == 2) Then
                    read(one_row(5:23),"(D19.12)") interim
                    IODE(Counter,Counter1) = interim
                    read(one_row(24:42),"(D19.12)") interim
                    Crs(Counter,Counter1) = interim
                    read(one_row(43:61),"(D19.12)") interim
                    Deltan(Counter,Counter1) = interim
                    read(one_row(62:80),"(D19.12)") interim
                    M0(Counter,Counter1) = interim
                Else If (Counter2 == 3) Then
                    read(one_row(5:23),"(D19.12)") interim
                    Cuc(Counter,Counter1) = interim
                    read(one_row(24:42),"(D19.12)") interim
                    e(Counter,Counter1) = interim
                    read(one_row(43:61),"(D19.12)") interim
                    Cus(Counter,Counter1) = interim
                    read(one_row(62:80),"(D19.12)") interim
                    Roota(Counter,Counter1) = interim
                Else If (Counter2 == 4) Then
                    read(one_row(5:23),"(D19.12)") interim
                    TOE(Counter,Counter1) = interim
                    read(one_row(24:42),"(D19.12)") interim
                    Cic(Counter,Counter1) = interim
                    read(one_row(43:61),"(D19.12)") interim
                    Omega0(Counter,Counter1) = interim
                    read(one_row(62:80),"(D19.12)") interim
                    Cis(Counter,Counter1) = interim
                Else If (Counter2 == 5) Then
                    read(one_row(5:23),"(D19.12)") interim
                    i0(Counter,Counter1) = interim
                    read(one_row(24:42),"(D19.12)") interim
                    Crc(Counter,Counter1) = interim
                    read(one_row(43:61),"(D19.12)") interim
                    Omega(Counter,Counter1) = interim
                    read(one_row(62:80),"(D19.12)") interim
                    OmegaDot(Counter,Counter1) = interim
                Else If (Counter2 == 6) Then
                    read(one_row(5:23),"(D19.12)") interim
                    IODE(Counter,Counter1) = Interim
                    read(one_row(43:61),"(D19.12)") Interim
                    Wn(Counter,Counter1) = Interim
                Else If (Counter2 == 7) Then
                    read(one_row(5:23),"(D19.12)") interim
                    Precise(Counter,Counter1) = interim
                    read(one_row(24:42),"(D19.12)") interim
                    Health(Counter,Counter1) = interim
                    read(one_row(43:61),"(D19.12)") interim
                    TGD(Counter,Counter1) = interim
                    read(one_row(62:80),"(D19.12)") interim
                    IODC(Counter,Counter1) = interim
                Else If (Counter2 == 8) Then
                    read(one_row(5:23),"(D19.12)") interim
                    StartSec(Counter,Counter1) = Interim
                    read(one_row(24:42),"(D19.12)") Interim
                    TLong(Counter,Counter1) = Interim
                    Cycle 
                Else
                End If
                read(unit = fileid2,fmt = "(A80)") one_row
            End Do
            
        Else If (one_row(1:1)== "G") Then
            Cycle
        Else If (one_row(1:1)== "R") Then
            Cycle
        Else If (one_row(1:1)== "E") Then
            Cycle
        Else If (one_row(1:1)== "S") Then
            Cycle
        Else If (one_row(1:1)== "J") Then
            Cycle
        End If
    End DO
    Close(Fileid2)
    
    !从C01卫星开始到C14卫星每隔30S内插一个坐标
    Do Sat_Num = 1,limit4,1                       !limit4 = 40 
        If (Sat_Num>Counter) Then
           Exit
        End If
        !计算从2017年9月8日0：00开始，每隔30s的卫星坐标
        Do Epoch = 1,limit2,1                     !limit2 = 2880
            T = 432000+30*(Epoch-1)
            Do Hour = 1,limit3,1                  !limit3 = 24
                Deltt = T-TOE(Sat_Num,Hour)
                IF (ABS(Deltt)<=1800) Then
                    Time = Hour
                End If
            End Do
            Sqrta = Roota(Sat_Num,Time)
            a = Sqrta**2
            n0 = Sqrt(GM/(a**3))
            n = n0+deltan(Sat_Num,Time)
            tk = T-TOE(Sat_NUm,Time)-14.0
            If (tk>302400) Then
                tk = tk-604800
            Else If (tk<-302400) Then
                tk = tk +604800
            End If
            !迭代计算，求出偏近点角Ek
            mk = M0(Sat_Num,Time)+n*tk
            ek1 = mk
            ek2 = 1E20
            DO While (ABS(ek2-ek1)>1E-20)
                ek2 = mk+e(Sat_Num,Time)*Sin(ek1)
                ek1 = ek2
            End Do
            !计算真近点角
            Ek = ek1
            Sinfk = (sqrt(1-e(Sat_Num,Time)**2)*Sin(Ek))/(1-e(Sat_Num,Time)*Cos(Ek))
            Cosfk = (Cos(Ek) - e(Sat_Num,Time))/(1-e(Sat_Num,Time)*Cos(Ek))
            fk = Atan2(Sinfk,Cosfk)
            !计算升角距
            phik = fk+Omega(Sat_Num,Time)
            !计算卫星轨道摄动项改正数
            deltauk = Cus(Sat_Num,Time)*Sin(2*phik)+Cuc(Sat_Num,Time)*Cos(2*phik)
            deltark = Crs(Sat_Num,Time)*Sin(2*phik)+Crc(Sat_Num,Time)*Cos(2*phik)
            deltaik = Cis(Sat_Num,Time)*Sin(2*phik)+Cic(Sat_Num,Time)*Cos(2*phik)
            !计算改正后的升交角距
            uk = phik+deltauk
            !计算改正后的向径
            rk = a*(1-e(Sat_Num,Time)*Cos(Ek))+deltark
            !计算改正后的倾角
            ik = i0(Sat_Num,Time)+deltaik+IDOT(Sat_Num,Time)*tk
            !计算卫星在轨道平面内的坐标
            xk = rk*Cos(uk)
            yk = rk*Sin(uk)
            If (SatSNR(Sat_Num,Time) == "C01" .Or. SatSNR(Sat_Num,Time) == "C02" .Or. SatSNR(Sat_Num,Time) == "C03" .Or. &
                SatSNR(Sat_Num,Time) == "C04" .Or. SatSNR(Sat_Num,Time) == "C05") Then
                Lk = Omega0(Sat_Num,Time)+OmegaDot(Sat_Num,Time)*tk - Ve*TOE(Sat_Num,Time)
                tX1 = xk*Cos(Lk)- yk*Cos(ik)*Sin(Lk)
                tY1 = xk*Sin(Lk)+ yk*Cos(ik)*Cos(Lk)
                tZ1 = yk*Sin(ik)
                delt_z = Ve*(tk)
                delt_x = -5*pi/180
                X(Sat_Num,Epoch) = tX1*Cos(delt_z)+tY1*Cos(delt_x)*Sin(delt_z)+tZ1*Sin(delt_x)*Sin(delt_z)
                Y(Sat_Num,Epoch) = (-tX1)*Sin(delt_z)+tY1*Cos(delt_x)*Cos(delt_z)+tZ1*Sin(delt_x)*Cos(delt_z)
                Z(Sat_Num,Epoch) = (-tY1)*Sin(delt_x)+tZ1*Cos(delt_x)
            Else
                Lk = Omega0(Sat_Num,Time)+(OmegaDot(Sat_Num,Time)-Ve)*tk - Ve*TOE(Sat_Num,Time)
                X(Sat_Num,Epoch) = xk * Cos(Lk) - yk * Cos(ik) * Sin(Lk)
                Y(Sat_Num,Epoch) = xk * Sin(Lk) + yk * Cos(ik) * Cos(Lk)
                Z(Sat_Num,Epoch) = yk * Sin(ik)
            End If
        End Do
    End Do
    
    Inquire(file = trim(filename3),exist = alive)     !!!!查询观测文件的状态
    
    !判断文件是否存在
    If (.not. alive) then
        Write(*,*)
        write(*,*)  "观测文件不存在，使用指令文件中的坐标！" !trim(filename3),"doesn't exist!"
    Else 
        Open(unit = fileid3,file = filename3)
        Write(*,*)
        Write(*,*) "正在从观测文件读取坐标..."
        Do While (.True.)
            Read(unit = fileid3,fmt = "(A60,A20)",iostat = status) Title_Detaile,Title
            !!判断文件是否处理完
            If (status /= 0) Then
                write(*,*)
                write(*,*) "坐标不存在,将使用指令文件中的坐标！"
                Exit
            End If
        
            If (Trim(Title) == "APPROX POSITION XYZ" ) Then
                Read(Title_Detaile(1:14),"(F14.4)") Interim
                Ref_1_X = Interim
                Read(Title_Detaile(15:28),"(F14.4)") Interim
                Ref_1_Y = Interim
                Read(Title_Detaile(29:42),"(F14.4)") Interim
                Ref_1_Z = Interim
                Write(*,*)
                Write(*,*) "坐标读取完成..."
                Exit
            End If
        End Do
    End If
    Close(Fileid3)
    
    !从C01卫星开始直到所有北斗卫星的所有内插30S后的坐标
    Do Sat_Num = 1,limit4,1
        If (Sat_Num>Counter) Then
           Exit 
        End If
        !计算从2017年9月8日0：00开始，每隔30s的卫星高度角
        Do Epoch = 1,limit2,1
            rx = X(Sat_Num,Epoch) - Ref_1_X
            ry = Y(Sat_Num,Epoch) - Ref_1_Y
            rz = Z(Sat_Num,Epoch) - Ref_1_Z
            rr0 = Sqrt(rx**2+ry**2+rz**2)
            theta = rr0/vel_light*earthrot
            rx = X(Sat_Num,Epoch) - Ref_1_X+Ref_1_Y
            ry = Y(Sat_Num,Epoch) - Ref_1_Y-Ref_1_X
            rz = Z(Sat_Num,Epoch) - Ref_1_Z
            rr = Sqrt(rx**2+ry**2+rz**2)
            Do While (Abs(rr0-rr)>0.001)
                rr0=Sqrt(rx**2+ry**2+rz**2)
                theta=rr0/vel_light*earthrot
                rx=X(Sat_Num,Epoch)-ref_1_X+theta*Ref_1_Y
                ry=Y(Sat_Num,Epoch)-ref_1_Y-theta*Ref_1_X
                rz=Z(Sat_Num,Epoch)-ref_1_Z
                rr=sqrt(rx**2+ry**2+rz**2)
            End Do
            Ref_1_Rox(Sat_Num,Epoch) = rx
            Ref_1_Roy(Sat_Num,Epoch) = ry
            Ref_1_Roz(Sat_Num,Epoch) = rz
            Ref_1_R0(Sat_Num,Epoch) = rr
        End Do
    End Do
    !计算卫星高度角
    Do Sat_Num = 1,limit4,1
        If (Sat_Num>Counter) Then
           Exit 
        End If
        !计算从2017年9月8日0：00开始，每隔30s的卫星坐标
        Do Epoch = 1,limit2,1
            dot=(ref_1_X*ref_1_Rox(Sat_Num,Epoch)+ref_1_Y*ref_1_Roy(Sat_Num,Epoch)+ref_1_Z*ref_1_Roz(Sat_Num,Epoch))
            mag1=(ref_1_X**2+ref_1_Y**2+ref_1_Z**2)
            mag2=ref_1_R0(Sat_Num,Epoch)**2
            dot=dot/sqrt(mag1*mag2)
            Ref_1_elev(Sat_Num,Epoch)=acos(dot)*180.0/pi
            Ref_1_elev(Sat_Num,Epoch)=90.0-ref_1_elev(Sat_Num,Epoch)
        End Do
    End Do
    !卫星高度角输出
    Open(unit = fileid4,file = filename4)
    Do Epoch = 1,limit2,1
        Str = ""
        Str_First = ""
        Do Sat_Num = 1,limit4,1
            If (Sat_Num>Counter) Then
                Exit
            End If
            If (Epoch == 1) Then
                Interim_First = SatSNR(Sat_Num,Epoch)
                Write(Interim_Str,"(A9)") Interim_First
                Str_First = Trim(Str_First)//Interim_Str
            End If
            If (Epoch == 1.And.Sat_Num == Counter) Then
                Write(unit = fileid4,fmt ="(5(A80))") Str_First(1:80),Str_First(81:160),Str_First(161:240),&
                Str_First(241:320),Str_First(321:400)
            End If
            If (Ref_1_elev(Sat_Num,Epoch)>0) Then
                Interim = Ref_1_elev(Sat_Num,Epoch)
                Write(Interim_Str,"(F9.4)") Interim
                Str = Trim(Str)//Interim_Str
            Else
                Str = Trim(Str)//Empty_Str
            End If
        End Do
        Write(unit = fileid4,fmt ="(5(A80))") Str(1:80),Str(81:160),Str(161:240),Str(241:320),Str(321:400)
    End Do
    Write(*,*)
    Write(*,*)"卫星高度角计算完成:",Trim(filename4)
    Close(fileid4)
    
    !对观测文件的操作，以信号（Pseudo Range、Carrier Phase、Doppler、Signal Strength）为单位建立数组，
    !数组第一维（横坐标）为卫星序号，第二维（纵坐标）为历元，由于不同IGS站对观测信号排列顺序不尽相同，
    !在次通过对站名的判断来实现数据的读取。
    inquire(file = trim(filename3),exist = alive)     !!!!查询观测文件的状态
    !判断观测文件是否存在
    If (.not. alive) then
        write(*,*) "观测文件不存在!"
        read(*,*)
        stop
    End If
    
    !打开观测文件
    Open(unit = fileid3,file = filename3)
    
    Write(*,*)
    write(*,*)"观测文件正在处理..."
    Do While(.True.)
        read(unit = fileid3,fmt = "(A60,A20)") Title_Detaile,Title
        
        If (Trim(Title) == "SYS / # / OBS TYPES") Then
            SatName = Title_Detaile(1:1)
            Read(Title_Detaile(3:5),"(I3)") TypeNum
            !观测信号的排列方式
            If (SatName == "C" .And.TypeNum == 12) Then
                ObsTypes = Trim(Title_Detaile(7:60))
            End If
            !If()
            !Cycle 
        !Else If (Trim(Title) == "TIME SYSTEM CORR") Then
        !    Cycle 
        Else IF (Trim(Title) == "END OF HEADER") Then
            Exit 
        End If
    End Do
    
    Epoch = 0
    
    Do while (.True.)
        !打开待处理文件
        Read(unit = fileid3,fmt = "(A323)",iostat = status) DataRow
        
        !判断文件是否处理完
        If (status /= 0) Then
            Write(*,*)
            Write(*,*) "观测文件处理完成..."
            Exit
        End If
        
        If (DataRow(1:1) == ">") Then
            
            Epoch = Epoch + 1
            Cycle
        Else If (DataRow(1:1) == "C") Then
            Read(DataRow(2:3),"(I2)") TypeNum
            Read(DataRow(4:17),"(F14.3)") Interim_Int
            C_1_PR(TypeNum,Epoch) = Interim_Int
            Read(DataRow(20:33),"(F14.3)") Interim_Int
            C_3_PR(TypeNum,Epoch) = Interim_Int
            Read(DataRow(36:49),"(F14.3)") Interim_Int
            C_2_PR(TypeNum,Epoch) = Interim_Int
            Read(DataRow(52:65),"(F14.3)") Interim_Int
            C_1_Do(TypeNum,Epoch) = Interim_Int
            Read(DataRow(68:81),"(F14.3)") Interim_Int
            C_3_Do(TypeNum,Epoch) = Interim_Int
            Read(DataRow(84:97),"(F14.3)") Interim_Int
            C_2_Do(TypeNum,Epoch) = Interim_Int
            Read(DataRow(100:113),"(F14.3)") Interim_Int
            C_1_CP(TypeNum,Epoch) = Interim_Int
            Read(DataRow(116:129),"(F14.3)") Interim_Int
            C_3_CP(TypeNum,Epoch) = Interim_Int
            Read(DataRow(132:145),"(F14.3)") Interim_Int
            C_2_CP(TypeNum,Epoch) = Interim_Int
            Read(DataRow(148:161),"(F14.3)") Interim_Int
            C_1_SS(TypeNum,Epoch) = Interim_Int
            Read(DataRow(164:177),"(F14.3)") Interim_Int
            C_3_SS(TypeNum,Epoch) = Interim_Int
            Read(DataRow(180:193),"(F14.3)") Interim_Int
            C_2_SS(TypeNum,Epoch) = Interim_Int
        End If
    End Do
    
    Write(*,*) C_1_PR(1,1)
    Write(*,*)
    write(*,*) "Press any key to exit!"
    Read(*,*)
    
End  Program Main