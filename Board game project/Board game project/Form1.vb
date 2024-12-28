Public Class Form1
    Dim NormalSpaces() As Label
    Dim HallwaySpaces() As Label
    Dim TimeCellSpaces() As Label
    Dim AllSpaces() As Label
    Dim pturn As Integer
    Dim pspots(3) As Integer
    Dim pTspots(3) As Integer
    Dim pHspots(3) As Integer
    Dim laps(3) As Integer
    Dim roll As Integer
    Dim rnum As New Random
    Dim pNames(3) As String
    Dim pMoney(3) As Integer
    Dim GotDoorKey(3) As Boolean
    Dim InTimeCell(3) As Boolean
    Dim TimeCellAns(3) As String
    Dim pBank() As Label
    Dim rnum2 As New Random
    Dim spaceDraw(3) As Integer
    Dim GotInvis(3) As Boolean
    Dim GotRay(3) As Boolean
    Dim InHallway(3) As Boolean
    Dim timeLeft As Integer
    Dim lightsOut As Boolean
    Dim lblLaps() As Label
    Dim elecAns As String
    Dim spacesFromExit(3) As Integer
    Dim firstDone As Boolean
    Dim secondDone As Boolean
    Dim thirdDone As Boolean
    Dim p1done As Boolean
    Dim p2done As Boolean
    Dim p3done As Boolean
    Dim p4done As Boolean



    Private Sub Form1_Load(sender As Object, e As EventArgs) Handles MyBase.Load
        NormalSpaces = {s0, s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12, s13, s14, s15, s16, s17, s18, s19, s20, s21, s22, s23}
        HallwaySpaces = {s9, HS1, HS2, HS3, s15}
        TimeCellSpaces = {TS0, TS1, TS2, TS3, TS4, TS5, TS6, TS7, TS8, TS9, TS10, TS11, TS12, TS13, TS14, TS15}
        pBank = {P1Money, P2Money, P3Money, P4Money}
        lblLaps = {p1Laps, p2Laps, p3Laps, p4Laps}
        AllSpaces = {s0, s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12, s13, s14, s15, s16, s17, s18, s19, s20, s21, s22, s23, HS1, HS2, HS3, TS1, TS2, TS3, TS4, TS5, TS6, TS7, TS8, TS9, TS10, TS11, TS12, TS13, TS14}
        pturn = 0
        lightsOut = False
        For i = 0 To 3
            pspots(i) = 0
            pMoney(i) = 500
            GotDoorKey(i) = False
            InTimeCell(i) = False
            spaceDraw(i) = 0
            GotInvis(i) = False
            GotRay(i) = False
            InHallway(i) = False
        Next
    End Sub

    Private Sub RulesToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles RulesToolStripMenuItem.Click
        Me.Hide()
        Form2.Show()
    End Sub

    Private Sub Button1_Click(sender As Object, e As EventArgs) Handles Button1.Click
        'I believe I earned a A on this project
        'I believe this because I have completed most of the requirement for a A+ and all the other requirements
        'I believe I earned a A in this class


        roll = rnum.Next(1, 7)
        lblRoll.Text = roll
        If InTimeCell(pturn) = False And InHallway(pturn) = False Then
            pspots(pturn) = pspots(pturn) + roll
        ElseIf InTimeCell(pturn) = True Then
            pTspots(pturn) = pTspots(pturn) + roll
        ElseIf InHallway(pturn) = True Then
            pHspots(pturn) = pHspots(pturn) + roll
        End If


        CheckLoop()
        movePiece()
        If lightsOut = False Then
            checkSpace()
        End If
        CheckWinEnd()
        changeTurns()

    End Sub
    Private Sub CheckLoop()
        If pspots(pturn) > 23 Then
            pspots(pturn) = pspots(pturn) - 24
            pMoney(pturn) = pMoney(pturn) + 100
            pBank(pturn).Text = "$" & pMoney(pturn)
            laps(pturn) = laps(pturn) + 1
            lblLaps(pturn).Text = laps(pturn) & " laps"
        End If
    End Sub
    Private Sub movePiece()
        MsgBox(pHspots(pturn))
        If InTimeCell(pturn) = False And InHallway(pturn) = False Then
            If pturn = 0 Then
                p0.Left = NormalSpaces(pspots(pturn)).Left
                p0.Top = NormalSpaces(pspots(pturn)).Top
            ElseIf pturn = 1 Then
                p1.Left = NormalSpaces(pspots(pturn)).Left + NormalSpaces(pspots(pturn)).Width - p1.Width
                p1.Top = NormalSpaces(pspots(pturn)).Top
            ElseIf pturn = 2 Then
                p3.Left = NormalSpaces(pspots(pturn)).Left + NormalSpaces(pspots(pturn)).Width - p3.Width
                p3.Top = NormalSpaces(pspots(pturn)).Top + NormalSpaces(pspots(pturn)).Height - p3.Height
            ElseIf pturn = 3 Then
                p2.Left = NormalSpaces(pspots(pturn)).Left
                p2.Top = NormalSpaces(pspots(pturn)).Top + NormalSpaces(pspots(pturn)).Height - p2.Height
            End If
        ElseIf InTimeCell(pturn) = True Then
            If pturn = 0 Then
                p0.Left = TimeCellSpaces(pTspots(pturn)).Left
                p0.Top = TimeCellSpaces(pTspots(pturn)).Top
            ElseIf pturn = 1 Then
                p1.Left = TimeCellSpaces(pTspots(pturn)).Left + TimeCellSpaces(pTspots(pturn)).Width - p1.Width
                p1.Top = TimeCellSpaces(pTspots(pturn)).Top
            ElseIf pturn = 2 Then
                p3.Left = TimeCellSpaces(pTspots(pturn)).Left + TimeCellSpaces(pTspots(pturn)).Width - p3.Width
                p3.Top = TimeCellSpaces(pTspots(pturn)).Top + TimeCellSpaces(pTspots(pturn)).Height - p3.Height
            ElseIf pturn = 3 Then
                p2.Left = TimeCellSpaces(pTspots(pturn)).Left
                p2.Top = TimeCellSpaces(pTspots(pturn)).Top + TimeCellSpaces(pTspots(pturn)).Height - p2.Height
            End If
        ElseIf InHallway(pturn) = True And pHspots(pturn) <= 3 Then
            If pturn = 0 Then
                p0.Left = HallwaySpaces(pHspots(pturn)).Left
                p0.Top = HallwaySpaces(pHspots(pturn)).Top
            ElseIf pturn = 1 Then
                p1.Left = HallwaySpaces(pHspots(pturn)).Left + HallwaySpaces(pHspots(pturn)).Width - p1.Width
                p1.Top = HallwaySpaces(pHspots(pturn)).Top
            ElseIf pturn = 2 Then
                p3.Left = HallwaySpaces(pHspots(pturn)).Left + HallwaySpaces(pHspots(pturn)).Width - p3.Width
                p3.Top = HallwaySpaces(pHspots(pturn)).Top + HallwaySpaces(pHspots(pturn)).Height - p3.Height
            ElseIf pturn = 3 Then
                p2.Left = HallwaySpaces(pHspots(pturn)).Left
                p2.Top = HallwaySpaces(pHspots(pturn)).Top + HallwaySpaces(pHspots(pturn)).Height - p2.Height
            End If
        ElseIf pTspots(pturn) >= 15 Then
            InTimeCell(pturn) = False
            pTspots(pturn) = 0
            pspots(pturn) = 6
            If pturn = 0 Then
                p0.Left = NormalSpaces(6).Left
                p0.Top = NormalSpaces(6).Top
            ElseIf pturn = 1 Then
                p1.Left = NormalSpaces(6).Left + NormalSpaces(6).Width - p1.Width
                p1.Top = NormalSpaces(6).Top
            ElseIf pturn = 2 Then
                p3.Left = NormalSpaces(6).Left + NormalSpaces(6).Width - p3.Width
                p3.Top = NormalSpaces(6).Top + NormalSpaces(6).Height - p3.Height
            ElseIf pturn = 3 Then
                p2.Left = NormalSpaces(6).Left
                p2.Top = NormalSpaces(6).Top + NormalSpaces(6).Height - p2.Height
            End If
        ElseIf InHallway(pturn) = True And pHspots(pturn) > 3 Then
            spacesFromExit(pturn) = pHspots(pturn) - 4
            If pturn = 0 Then
                p0.Left = NormalSpaces(15 + spacesFromExit(pturn)).Left
                p0.Top = NormalSpaces(15 + spacesFromExit(pturn)).Top
            ElseIf pturn = 1 Then
                p1.Left = NormalSpaces(15 + spacesFromExit(pturn)).Left + NormalSpaces(spacesFromExit(pturn)).Width - p1.Width
                p1.Top = NormalSpaces(15 + spacesFromExit(pturn)).Top
            ElseIf pturn = 2 Then
                p3.Left = NormalSpaces(15 + spacesFromExit(pturn)).Left + NormalSpaces(15 + spacesFromExit(pturn)).Width - p3.Width
                p3.Top = NormalSpaces(15 + spacesFromExit(pturn)).Top + NormalSpaces(15 + spacesFromExit(pturn)).Height - p3.Height
            ElseIf pturn = 3 Then
                p2.Left = NormalSpaces(15 + spacesFromExit(pturn)).Left
                p2.Top = NormalSpaces(15 + spacesFromExit(pturn)).Top + NormalSpaces(15 + spacesFromExit(pturn)).Height - p2.Height
            End If
            movePiece()
            InHallway(pturn) = False
            pHspots(pturn) = 0
        End If

    End Sub
    Private Sub checkSpace()

        If pspots(pturn) = 2 Or pspots(pturn) = 17 Or pspots(pturn) = 23 Then
            pMoney(pturn) = pMoney(pturn) + 50
            pBank(pturn).Text = "$" & pMoney(pturn)
        ElseIf pspots(pturn) = 3 Then
            GotDoorKey(pturn) = True
        ElseIf pspots(pturn) = 4 Or pspots(pturn) = 8 Or pspots(pturn) = 16 Or pspots(pturn) = 20 Then
            TimeCellAns(pturn) = InputBox("Do you want to bribe to guard for $100? yes or no")
            If TimeCellAns(pturn).ToLower = "yes" Then
                pMoney(pturn) = pMoney(pturn) - 100
                pBank(pturn).Text = "$" & pMoney(pturn)
            Else
                InTimeCell(pturn) = True
                pspots(pturn) = 6
                pTspots(pturn) = 0
                If pturn = 0 Then
                    p0.Left = TimeCellSpaces(0).Left
                    p0.Top = TimeCellSpaces(0).Top
                ElseIf pturn = 1 Then
                    p1.Left = TimeCellSpaces(0).Left + TimeCellSpaces(0).Width - p1.Width
                    p1.Top = TimeCellSpaces(0).Top
                ElseIf pturn = 2 Then
                    p3.Left = TimeCellSpaces(0).Left + TimeCellSpaces(0).Width - p3.Width
                    p3.Top = TimeCellSpaces(0).Top + TimeCellSpaces(0).Height - p3.Height
                ElseIf pturn = 3 Then
                    p2.Left = TimeCellSpaces(0).Left
                    p2.Top = TimeCellSpaces(0).Top + TimeCellSpaces(0).Height - p2.Height
                End If
            End If
        ElseIf pspots(pturn) = 7 Or pspots(pturn) = 19 Then
            pMoney(pturn) = pMoney(pturn) - 100
            pBank(pturn).Text = "$" & pMoney(pturn)
        ElseIf pspots(pturn) = 11 Or pspots(pturn) = 22 Or pTspots(pturn) = 7 Then
            SpaceCardPile.Show()
            spaceDraw(pturn) = rnum2.Next(1, 101)
            If spaceDraw(pturn) <= 20 Then
                SpaceCardPile.Text = "Give $10 to each player"
                pMoney(pturn) = pMoney(pturn) - 30
                If pturn = 0 Then
                    pMoney(1) = pMoney(1) + 10
                    pMoney(2) = pMoney(2) + 10
                    pMoney(3) = pMoney(3) + 10
                    For i = 0 To 3
                        pBank(i).Text = "$" & pMoney(i)
                    Next
                ElseIf pturn = 1 Then
                    pMoney(0) = pMoney(0) + 10
                    pMoney(2) = pMoney(2) + 10
                    pMoney(3) = pMoney(3) + 10
                    For i = 0 To 3
                        pBank(i).Text = "$" & pMoney(i)
                    Next
                ElseIf pturn = 2 Then
                    pMoney(0) = pMoney(0) + 10
                    pMoney(1) = pMoney(1) + 10
                    pMoney(3) = pMoney(3) + 10
                    For i = 0 To 3
                        pBank(i).Text = "$" & pMoney(i)
                    Next
                ElseIf pturn = 3 Then
                    pMoney(0) = pMoney(0) + 10
                    pMoney(2) = pMoney(2) + 10
                    pMoney(1) = pMoney(1) + 10
                    For i = 0 To 3
                        pBank(i).Text = "$" & pMoney(i)
                    Next
                End If
            ElseIf spaceDraw(pturn) > 20 And spaceDraw(pturn) <= 40 Then
                SpaceCardPile.Text = "Receive $20 from all players"
                pMoney(pturn) = pMoney(pturn) + 60
                If pturn = 0 Then
                    pMoney(1) = pMoney(1) - 20
                    pMoney(2) = pMoney(2) - 20
                    pMoney(3) = pMoney(3) - 20
                    For i = 0 To 3
                        pBank(i).Text = "$" & pMoney(i)
                    Next
                ElseIf pturn = 1 Then
                    pMoney(0) = pMoney(0) - 20
                    pMoney(2) = pMoney(2) - 20
                    pMoney(3) = pMoney(3) - 20
                    For i = 0 To 3
                        pBank(i).Text = "$" & pMoney(i)
                    Next
                ElseIf pturn = 2 Then
                    pMoney(0) = pMoney(0) - 20
                    pMoney(1) = pMoney(1) - 20
                    pMoney(3) = pMoney(3) - 20
                    For i = 0 To 3
                        pBank(i).Text = "$" & pMoney(i)
                    Next
                ElseIf pturn = 3 Then
                    pMoney(0) = pMoney(0) - 20
                    pMoney(2) = pMoney(2) - 20
                    pMoney(1) = pMoney(1) - 20
                    For i = 0 To 3
                        pBank(i).Text = "$" & pMoney(i)
                    Next
                End If
            ElseIf spaceDraw(pturn) > 40 And spaceDraw(pturn) <= 50 Then
                SpaceCardPile.Text = "Pay $50 to the bank"
                pMoney(pturn) = pMoney(pturn) - 50
                pBank(pturn).Text = "$" & pMoney(pturn)
            ElseIf spaceDraw(pturn) > 50 And spaceDraw(pturn) <= 65 Then
                SpaceCardPile.Text = "Collect $100 from the bank"
                pMoney(pturn) = pMoney(pturn) - 50
                pBank(pturn).Text = "$" & pMoney(pturn)
            ElseIf spaceDraw(pturn) > 65 And spaceDraw(pturn) <= 75 Then
                SpaceCardPile.Text = "You have received a invis cloak. Use it to sneak past one camera"
                GotInvis(pturn) = True
            ElseIf spaceDraw(pturn) > 75 And spaceDraw(pturn) <= 80 Then
                If laps(pturn) < 2 Then
                    laps(pturn) = laps(pturn) + 1
                    lblLaps(pturn).Text = laps(pturn) & " laps"
                    SpaceCardPile.Text = "You have been sent forward in time 1 loop"
                Else
                    SpaceCardPile.Text = "It seems the time machine has run out of gas. Nothing happens"
                End If
            ElseIf spaceDraw(pturn) > 80 And spaceDraw(pturn) <= 85 Then
                SpaceCardPile.Text = "You have been given a space ray. When you land on a guard space nothing happens. One time use"
                GotRay(pturn) = True
            ElseIf spaceDraw(pturn) > 85 And spaceDraw(pturn) <= 90 Then
                GotDoorKey(pturn) = True
                SpaceCardPile.Text = "You have been given a door key. Use it to potentially get good loot"
            ElseIf spaceDraw(pturn) > 90 And spaceDraw(pturn) <= 93 Then
                If laps(pturn) >= 1 Then
                    laps(pturn) = laps(pturn) - 1
                    lblLaps(pturn).Text = laps(pturn) & " laps"
                    SpaceCardPile.Text = "You have been sent back in time 1 loop"
                Else
                    SpaceCardPile.Text = "It seems the time machine has run out of gas. Nothing happens"
                End If
            ElseIf spaceDraw(pturn) > 93 Then
                    SpaceCardPile.Text = "A alert sounded and you have been sent to jail"
                    pTspots(pturn) = 0
                    If pturn = 0 Then
                        p0.Left = TimeCellSpaces(0).Left
                        p0.Top = TimeCellSpaces(0).Top
                    ElseIf pturn = 1 Then
                        p1.Left = TimeCellSpaces(0).Left + TimeCellSpaces(0).Width - p1.Width
                        p1.Top = TimeCellSpaces(0).Top
                    ElseIf pturn = 2 Then
                        p3.Left = TimeCellSpaces(0).Left + TimeCellSpaces(0).Width - p3.Width
                        p3.Top = TimeCellSpaces(0).Top + TimeCellSpaces(0).Height - p3.Height
                    ElseIf pturn = 3 Then
                        p2.Left = TimeCellSpaces(0).Left
                        p2.Top = TimeCellSpaces(0).Top + TimeCellSpaces(0).Height - p2.Height

                    End If
            End If
        ElseIf pHspots(pturn) = 1 Then
            If laps(pturn) < 2 Then
                laps(pturn) = laps(pturn) + 1
                lblLaps(pturn).Text = laps(pturn) & " laps"
            End If
        ElseIf pspots(pturn) = 9 Then
            If GotDoorKey(pturn) = True Then
                InHallway(pturn) = True
                pHspots(pturn) = 0
            End If
        ElseIf pHspots(pturn) = 2 Then
            pMoney(pturn) = pMoney(pturn) + 200
            pBank(pturn).Text = "$" & pMoney(pturn)
        ElseIf pHspots(pturn) = 3 Then
            GotRay(pturn) = True
        ElseIf pTspots(pturn) = 3 Or pTspots(pturn) = 9 Or pTspots(pturn) = 14 Then
            pTspots(pturn) = pTspots(pturn) - 3
            If pturn = 0 Then
                p0.Left = TimeCellSpaces(pTspots(pturn)).Left
                p0.Top = TimeCellSpaces(pTspots(pturn)).Top
            ElseIf pturn = 1 Then
                p1.Left = TimeCellSpaces(pTspots(pturn)).Left + TimeCellSpaces(pTspots(pturn)).Width - p1.Width
                p1.Top = TimeCellSpaces(pTspots(pturn)).Top
            ElseIf pturn = 2 Then
                p3.Left = TimeCellSpaces(pTspots(pturn)).Left + TimeCellSpaces(pTspots(pturn)).Width - p3.Width
                p3.Top = TimeCellSpaces(pTspots(pturn)).Top + TimeCellSpaces(pTspots(pturn)).Height - p3.Height
            ElseIf pturn = 3 Then
                p2.Left = TimeCellSpaces(pTspots(pturn)).Left
                p2.Top = TimeCellSpaces(pTspots(pturn)).Top + TimeCellSpaces(pTspots(pturn)).Height - p2.Height
            End If
        ElseIf pTspots(pturn) = 11 Or pTspots(pturn) = 5 Then
            pTspots(pturn) = 0
            If pturn = 0 Then
                p0.Left = TimeCellSpaces(pTspots(pturn)).Left
                p0.Top = TimeCellSpaces(pTspots(pturn)).Top
            ElseIf pturn = 1 Then
                p1.Left = TimeCellSpaces(pTspots(pturn)).Left + TimeCellSpaces(pTspots(pturn)).Width - p1.Width
                p1.Top = TimeCellSpaces(pTspots(pturn)).Top
            ElseIf pturn = 2 Then
                p3.Left = TimeCellSpaces(pTspots(pturn)).Left + TimeCellSpaces(pTspots(pturn)).Width - p3.Width
                p3.Top = TimeCellSpaces(pTspots(pturn)).Top + TimeCellSpaces(pTspots(pturn)).Height - p3.Height
            ElseIf pturn = 3 Then
                p2.Left = TimeCellSpaces(pTspots(pturn)).Left
                p2.Top = TimeCellSpaces(pTspots(pturn)).Top + TimeCellSpaces(pTspots(pturn)).Height - p2.Height
            End If
        ElseIf pTspots(pturn) = 12 Then
            elecAns = InputBox("Do you want to sabotage the ligths for $100. yes or no")
            If elecAns.ToLower = "yes" Then
                timeLeft = 15
                Timer1.Start()
                Background.BackColor = Color.Black
                Label1.BackColor = Color.Black
                For i = 0 To 40
                    AllSpaces(i).BackColor = Color.DarkGray
                Next
            End If

        End If
    End Sub
    Private Sub changeTurns()
        pturn = pturn + 1
        If pturn > 3 Then
            pturn = 0
        End If
        If p1done = True And pturn = 0 Then
            pturn = 1
        ElseIf p2done = True And pturn = 1 Then
            pturn = 2
        ElseIf p3done = True And pturn = 2 Then
            pturn = 3
        ElseIf p4done = True And pturn = 3 Then
            pturn = 0
        End If
        lblTurn.Text = pNames(pturn)
    End Sub

    Private Sub BeginGameToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles BeginGameToolStripMenuItem.Click
        Button1.Enabled = True
        BeginGameToolStripMenuItem.Enabled = False
        ResetGameToolStripMenuItem.Enabled = True
        pNames(pturn) = InputBox("Player 1 Name:")
        PName1.Text = pNames(pturn)
        changeTurns()
        pNames(pturn) = InputBox("Player 2 Name:")
        PName2.Text = pNames(pturn)
        changeTurns()
        pNames(pturn) = InputBox("Player 3 Name:")
        PName3.Text = pNames(pturn)
        changeTurns()
        pNames(pturn) = InputBox("Player 4 Name:")
        PName4.Text = pNames(pturn)
        changeTurns()
        Label3.Show()
        lblTurn.Text = pNames(pturn)
        Label2.Hide()
        Label4.Hide()
        Label5.Hide()
        Label6.Hide()
        Label7.Hide()
        Label8.Hide()
        Label9.Hide()
        Label11.Hide()
    End Sub

    Private Sub Timer1_Tick(sender As Object, e As EventArgs) Handles Timer1.Tick
        timeLeft = timeLeft - 1
        lightsOut = True
        If timeLeft < 1 Then
            Timer1.Stop()
            lightsOut = False
            Background.BackColor = Color.MidnightBlue
            Label1.BackColor = Color.DarkOliveGreen
            For i = 0 To 40
                AllSpaces(i).BackColor = Color.Moccasin
            Next
        End If
    End Sub

    Private Sub ResetGameToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles ResetGameToolStripMenuItem.Click
        pturn = 0
        Button1.Enabled = False
        BeginGameToolStripMenuItem.Enabled = True
        ResetGameToolStripMenuItem.Enabled = False
        p0.Left = NormalSpaces(0).Left
        p0.Top = NormalSpaces(0).Top

        p1.Left = NormalSpaces(0).Left + NormalSpaces(0).Width - p1.Width
        p1.Top = NormalSpaces(0).Top

        p3.Left = NormalSpaces(0).Left + NormalSpaces(0).Width - p3.Width
        p3.Top = NormalSpaces(0).Top + NormalSpaces(0).Height - p3.Height

        p2.Left = NormalSpaces(0).Left
        p2.Top = NormalSpaces(0).Top + NormalSpaces(0).Height - p2.Height
        p1done = False
        p2done = False
        p3done = False
        p4done = False
        firstDone = False
        secondDone = False
        thirdDone = False

        For i = 0 To 3
            pspots(i) = 0

            pMoney(i) = 500
            pBank(i).Text = "$" & pMoney(i)
            GotDoorKey(i) = False
            InTimeCell(i) = False
            spaceDraw(i) = 0
            GotInvis(i) = False
            GotRay(i) = False
            laps(i) = 0
            lblLaps(pturn).Text = laps(pturn) & " laps"
            InHallway(i) = False
            lightsOut = False
        Next
    End Sub
    Private Sub CheckWinEnd()
        If laps(pturn) > 2 Then
            If firstDone = False Then
                Label6.Text = Name(pturn)
                Label6.Show()
                Label2.Show()
                If pturn = 0 Then
                    p1done = True
                ElseIf pturn = 1 Then
                    p2done = True
                ElseIf pturn = 2 Then
                    p3done = True
                ElseIf pturn = 3 Then
                    p4done = True
                End If
                firstDone = True
            ElseIf firstDone = True And secondDone = False Then
                Label5.Text = Name(pturn)
                Label4.Show()
                Label5.Show()
                If pturn = 0 Then
                    p1done = True
                ElseIf pturn = 1 Then
                    p2done = True
                ElseIf pturn = 2 Then
                    p3done = True
                ElseIf pturn = 3 Then
                    p4done = True
                End If
                secondDone = True
            ElseIf firstDone = True And secondDone = True And thirdDone = False Then
                Label11.Text = Name(pturn)
                Label8.Show()
                Label11.Show()
                If pturn = 0 Then
                    p1done = True
                ElseIf pturn = 1 Then
                    p2done = True
                ElseIf pturn = 2 Then
                    p3done = True
                ElseIf pturn = 3 Then
                    p4done = True
                End If
                thirdDone = True
            Else

                Label7.Text = Name(pturn)
                Label7.Show()
                If pturn = 0 Then
                    p1done = True
                ElseIf pturn = 1 Then
                    p2done = True
                ElseIf pturn = 2 Then
                    p3done = True
                ElseIf pturn = 3 Then
                    p4done = True
                End If
                Label9.Show()
            End If
            If p1done = True And p2done = True And p3done = True And p4done = True Then
                pturn = 0
                Button1.Enabled = False
                BeginGameToolStripMenuItem.Enabled = True
                ResetGameToolStripMenuItem.Enabled = False
                p0.Left = NormalSpaces(0).Left
                p0.Top = NormalSpaces(0).Top

                p1.Left = NormalSpaces(0).Left + NormalSpaces(0).Width - p1.Width
                p1.Top = NormalSpaces(0).Top

                p3.Left = NormalSpaces(0).Left + NormalSpaces(0).Width - p3.Width
                p3.Top = NormalSpaces(0).Top + NormalSpaces(0).Height - p3.Height

                p2.Left = NormalSpaces(0).Left
                p2.Top = NormalSpaces(0).Top + NormalSpaces(0).Height - p2.Height
                p1done = False
                p2done = False
                p3done = False
                p4done = False
                firstDone = False
                secondDone = False
                thirdDone = False

                For i = 0 To 3
                    pspots(i) = 0

                    pMoney(i) = 500
                    pBank(i).Text = "$" & pMoney(i)
                    GotDoorKey(i) = False
                    InTimeCell(i) = False
                    spaceDraw(i) = 0
                    GotInvis(i) = False
                    GotRay(i) = False
                    laps(i) = 0
                    lblLaps(pturn).Text = laps(pturn) & " laps"
                    InHallway(i) = False
                    lightsOut = False
                Next
            End If
        End If
    End Sub
End Class

