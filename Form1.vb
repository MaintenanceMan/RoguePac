Public Class Form1
    Private Const gEMPTY = " "
    Private Const gWALL = "#"
    Private Const gPLAYER = "@"
    Private Const gGHOST = "G"
    Private Const gGHOST_AFRAID = "g"
    Private Const gPELLET = "x"

    Private Const gCELL_SIZE = 16
    Private Const gSTART_HIT_POINTS = 10
    Private Const gREGNERATE_HP = 1
    Private Const gPLAYER_DAMAGE = 5
    Private Const gGHOST_DAMAGE = 4
    Private Const gNUM_GHOSTS = 4
    Private Const gGHOST_EXP = 10
    Private Const gNUM_PELLETS = 4
    Private Const gGHOST_RUN_DURATION = 50
    Private Const gMAX_ROWS = 24
    Private Const gMAX_COLS = 48
    Private Const gLEVEL_UP_AT = 100

    Dim gArrMaze(gMAX_ROWS - 1, gMAX_COLS - 1) As Char

    Dim gPlayerX As Integer
    Dim gPlayerY As Integer
    Dim gPlayerHP As Integer
    Dim gPlayerMaxHP As Integer
    Dim gPlayerLvl As Integer
    Dim gPlayerExp As Integer
    Dim gExpLevelUp As Integer
    Dim gPlaying As Boolean

    Private Structure Ghost
        Dim nX As Integer
        Dim nY As Integer
        Dim nHP As Integer
    End Structure

    Dim gArrGhosts(gNUM_GHOSTS) As Ghost
    Dim gCountDown As Integer

    Dim gStrMessage As String

    Private Sub InitMessage()
        gStrMessage = ""
    End Sub

    Private Sub AddMessage(ByRef strMsg As String)
        If (gStrMessage.Length = 0) Then
            gStrMessage = strMsg
        Else
            gStrMessage = gStrMessage + " " + strMsg
        End If
    End Sub

    Private Sub InitMaze()
        ' Blank out the whole maze
        For nRow = 0 To gMAX_ROWS - 1
            For nCol = 0 To gMAX_COLS - 1
                gArrMaze(nRow, nCol) = gEMPTY
            Next
        Next
        ' Create a top and bottom walls
        For nCol = 0 To gMAX_COLS - 1
            gArrMaze(0, nCol) = gWALL
            gArrMaze(gMAX_ROWS - 1, nCol) = gWALL
        Next
        ' Create a left and right walls
        For nRow = 0 To gMAX_ROWS - 1
            gArrMaze(nRow, 0) = gWALL
            gArrMaze(nRow, gMAX_COLS - 1) = gWALL
        Next
        ' Add random walls
        Randomize()
        For nWall = 0 To gMAX_ROWS * gMAX_COLS / 4
            Dim nX = CInt(Int(gMAX_COLS * Rnd()))
            Dim nY = CInt(Int(gMAX_ROWS * Rnd()))
            gArrMaze(nY, nX) = gWALL
        Next
    End Sub

    Private Sub InitPlayer()
        Dim nX As Integer
        Dim nY As Integer
        Dim bPlaced As Boolean
        bPlaced = False
        Do Until bPlaced
            ' Propose a random location to start player
            nX = CInt(Int(gMAX_COLS * Rnd()))
            nY = CInt(Int(gMAX_ROWS * Rnd()))
            ' Check if that location is occupied
            bPlaced = (gArrMaze(nY, nX) = gEMPTY)
        Loop
        gPlayerX = nX
        gPlayerY = nY
        gArrMaze(gPlayerY, gPlayerX) = gPLAYER
        gPlayerHP = gSTART_HIT_POINTS
        gPlayerMaxHP = gSTART_HIT_POINTS
        gPlayerLvl = 1
        gPlayerExp = 0
        gExpLevelUp = gLEVEL_UP_AT
        gPlaying = True
        InitMessage()
        AddMessage("Welcome to RoguePac version " + GetVersion())
    End Sub

    Private Sub InitGhost(ByVal nIndex As Integer)
        Dim nX As Integer
        Dim nY As Integer
        Dim bPlaced As Boolean
        bPlaced = False
        Do Until bPlaced
            ' Propose a random location to start ghost
            nX = CInt(Int(gMAX_COLS * Rnd()))
            nY = CInt(Int(gMAX_ROWS * Rnd()))
            ' Check if that location is occupied
            bPlaced = (gArrMaze(nY, nX) = gEMPTY)
        Loop
        gArrGhosts(nIndex).nX = nX
        gArrGhosts(nIndex).nY = nY
        If (gCountDown > 0) Then
            gArrMaze(gArrGhosts(nIndex).nY, gArrGhosts(nIndex).nX) = gGHOST_AFRAID
        Else
            gArrMaze(gArrGhosts(nIndex).nY, gArrGhosts(nIndex).nX) = gGHOST
        End If
        gArrGhosts(nIndex).nHP = gSTART_HIT_POINTS
    End Sub

    Private Sub InitPellets()
        Dim nX As Integer
        Dim nY As Integer
        Dim bPlaced As Boolean
        For nPellet = 0 To gNUM_PELLETS
            bPlaced = False
            Do Until bPlaced
                ' Propose a random location to put pellet
                nX = CInt(Int(gMAX_COLS * Rnd()))
                nY = CInt(Int(gMAX_ROWS * Rnd()))
                ' Check if that location is occupied
                bPlaced = (gArrMaze(nY, nX) = gEMPTY)
            Loop
            gArrMaze(nY, nX) = gPELLET
        Next
    End Sub

    Private Sub InitGhosts()
        For nGhost = 0 To gNUM_GHOSTS - 1
            InitGhost(nGhost)
        Next
        gCountDown = 0
    End Sub

    Private Sub ErasePlayer()
        gArrMaze(gPlayerY, gPlayerX) = gEMPTY
    End Sub

    Private Sub EraseGhost(ByVal nIndex)
        gArrMaze(gArrGhosts(nIndex).nY, gArrGhosts(nIndex).nX) = gEMPTY
    End Sub

    Private Function CanMove(ByVal nX, ByVal nY) As Boolean
        Dim bCanMove As Boolean
        bCanMove = False
        If (gArrMaze(nY, nX) = gEMPTY) Or
            (gArrMaze(nY, nX) = gPELLET) Then
            bCanMove = True
        End If
        Return bCanMove
    End Function

    Private Function IsGhost(ByVal nX, ByVal nY) As Boolean
        Dim bIsGhost As Boolean
        bIsGhost = False
        If (gArrMaze(nY, nX) = gGHOST) Or
            (gArrMaze(nY, nX) = gGHOST_AFRAID) Then
            bIsGhost = True
        End If
        Return bIsGhost
    End Function

    Private Function IsPlayer(ByVal nX, ByVal nY) As Boolean
        Dim bIsPlayer As Boolean
        bIsPlayer = False
        If (gArrMaze(nY, nX) = gPLAYER) Then
            bIsPlayer = True
        End If
        Return bIsPlayer
    End Function

    Private Sub Form1_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load
        Dim dlg As New Intro()
        dlg.ShowDialog()
        InitMaze()
        InitPlayer()
        InitGhosts()
        InitPellets()
    End Sub

    Private Sub Form1_Paint(ByVal sender As System.Object, ByVal e As System.Windows.Forms.PaintEventArgs) Handles MyBase.Paint
        e.Graphics.Clear(Color.Black)
        Dim fontObj As Font
        fontObj = New System.Drawing.Font("Courier New", 14, FontStyle.Regular)
        ' Load in images.
        ' TO DO: load only once at prog startup.
        Dim imgWall As Image = Image.FromFile("wall.png")
        Dim imgBigGhost As Image = Image.FromFile("ghostbig.png")
        Dim imgSmallGhost As Image = Image.FromFile("ghostsmall.png")
        Dim imgPlayer As Image = Image.FromFile("player.png")
        Dim imgPellet As Image = Image.FromFile("pellet.png")
        ' Show the maze
        For nRow = 0 To gMAX_ROWS - 1
            For nCol = 0 To gMAX_COLS - 1
                If gArrMaze(nRow, nCol) = gWALL Then
                    e.Graphics.DrawImage(imgWall, nCol * gCELL_SIZE, nRow * gCELL_SIZE)
                ElseIf gArrMaze(nRow, nCol) = gGHOST Then
                    e.Graphics.DrawImage(imgBigGhost, nCol * gCELL_SIZE, nRow * gCELL_SIZE)
                ElseIf gArrMaze(nRow, nCol) = gGHOST_AFRAID Then
                    e.Graphics.DrawImage(imgSmallGhost, nCol * gCELL_SIZE, nRow * gCELL_SIZE)
                ElseIf gArrMaze(nRow, nCol) = gPLAYER Then
                    e.Graphics.DrawImage(imgPlayer, nCol * gCELL_SIZE, nRow * gCELL_SIZE)
                ElseIf gArrMaze(nRow, nCol) = gPELLET Then
                    e.Graphics.DrawImage(imgPellet, nCol * gCELL_SIZE, nRow * gCELL_SIZE)
                End If
            Next
        Next
        ' Show stats
        Dim strStats As String
        strStats = "Level:" + CStr(gPlayerLvl) + "  Exp:" + CStr(gPlayerExp) + "  HP:" + CStr(gPlayerHP)
        e.Graphics.DrawString(strStats, fontObj, Brushes.White, 0, gMAX_ROWS * 16)
        ' Show the latest messagesv
        e.Graphics.DrawString(gStrMessage, fontObj, Brushes.White, 0, (gMAX_ROWS + 1) * 16)
    End Sub

    Private Sub MoveGhost(ByVal nIndex As Integer, ByVal nX As Integer, ByVal nY As Integer)
        EraseGhost(nIndex)
        gArrGhosts(nIndex).nX = nX
        gArrGhosts(nIndex).nY = nY
        If (gCountDown > 0) Then
            gArrMaze(gArrGhosts(nIndex).nY, gArrGhosts(nIndex).nX) = gGHOST_AFRAID
        Else
            gArrMaze(gArrGhosts(nIndex).nY, gArrGhosts(nIndex).nX) = gGHOST
        End If
    End Sub

    Private Sub HitPlayer()
        gPlayerHP = gPlayerHP - gGHOST_DAMAGE
        AddMessage("The ghost hits you.")
        If (gPlayerHP <= 0) Then
            gPlaying = False
            ' Make sure the death message does not scroll off to the right
            InitMessage()
            AddMessage("You die.")
        End If
    End Sub

    Private Sub ProcessGhost(ByVal nIndex As Integer)
        Dim nNewX As Integer
        Dim nNewY As Integer
        nNewX = gArrGhosts(nIndex).nX
        nNewY = gArrGhosts(nIndex).nY
        ' Determine optimal move toward player
        If (gPlayerX > gArrGhosts(nIndex).nX) Then
            nNewX = nNewX + 1
        ElseIf (gPlayerX < gArrGhosts(nIndex).nX) Then
            nNewX = nNewX - 1
        End If
        If (gPlayerY > gArrGhosts(nIndex).nY) Then
            nNewY = nNewY + 1
        ElseIf (gPlayerY < gArrGhosts(nIndex).nY) Then
            nNewY = nNewY - 1
        End If
        If CanMove(nNewX, nNewY) Then
            MoveGhost(nIndex, nNewX, nNewY)
        ElseIf IsPlayer(nNewX, nNewY) Then
            HitPlayer()
        Else
            ' Determine just horizontal move toward player
            nNewX = gArrGhosts(nIndex).nX
            nNewY = gArrGhosts(nIndex).nY
            If (gPlayerX > gArrGhosts(nIndex).nX) Then
                nNewX = nNewX + 1
            ElseIf (gPlayerX < gArrGhosts(nIndex).nX) Then
                nNewX = nNewX - 1
            End If
            If CanMove(nNewX, nNewY) Then
                MoveGhost(nIndex, nNewX, nNewY)
            ElseIf IsPlayer(nNewX, nNewY) Then
                HitPlayer()
            Else
                ' Determine just vertical move toward player
                nNewX = gArrGhosts(nIndex).nX
                nNewY = gArrGhosts(nIndex).nY
                If (gPlayerY > gArrGhosts(nIndex).nY) Then
                    nNewY = nNewY + 1
                ElseIf (gPlayerY < gArrGhosts(nIndex).nY) Then
                    nNewY = nNewY - 1
                End If
                If CanMove(nNewX, nNewY) Then
                    MoveGhost(nIndex, nNewX, nNewY)
                ElseIf IsPlayer(nNewX, nNewY) Then
                    HitPlayer()
                End If
            End If
        End If
    End Sub

    Private Sub RedrawGhosts()
        For nGhost = 0 To gNUM_GHOSTS - 1
            If (gCountDown > 0) Then
                gArrMaze(gArrGhosts(nGhost).nY, gArrGhosts(nGhost).nX) = gGHOST_AFRAID
            Else
                gArrMaze(gArrGhosts(nGhost).nY, gArrGhosts(nGhost).nX) = gGHOST
            End If
        Next
    End Sub

    Private Sub ProcessGhosts()
        For nGhost = 0 To gNUM_GHOSTS - 1
            If gPlaying Then
                ' Only let ghosts attack if player still lives
                ProcessGhost(nGhost)
            End If
        Next
    End Sub

    Private Sub Regenerate()
        gPlayerHP = gPlayerHP + gREGNERATE_HP
        If (gPlayerHP > gPlayerMaxHP) Then
            gPlayerHP = gPlayerMaxHP
        End If
    End Sub

    Private Function FindGhost(ByVal nX As Integer, ByVal nY As Integer) As Integer
        Dim nIndex As Integer
        ' Assume we cannot find the ghost
        nIndex = -1
        For nGhost = 0 To gNUM_GHOSTS - 1
            If (gArrGhosts(nGhost).nX = nX) And
                (gArrGhosts(nGhost).nY = nY) Then
                nIndex = nGhost
            End If
        Next
        Return nIndex
    End Function

    Private Sub HitGhost(ByVal nX As Integer, ByVal nY As Integer)
        Dim nIndex As Integer
        nIndex = FindGhost(nX, nY)
        If (nIndex <> -1) Then
            gArrGhosts(nIndex).nHP = gArrGhosts(nIndex).nHP - gPLAYER_DAMAGE
            If (gCountDown > 0) Then
                ' When ghosts are scared, one hit kills them.
                gArrGhosts(nIndex).nHP = 0
            End If
            AddMessage("You hit the ghost.")
            If (gArrGhosts(nIndex).nHP <= 0) Then
                EraseGhost(nIndex)
                InitGhost(nIndex)
                AddMessage("You kill the ghost.")
                gPlayerExp = gPlayerExp + gGHOST_EXP
                If (gPlayerExp > gExpLevelUp) Then
                    gPlayerLvl = gPlayerLvl + 1
                    gExpLevelUp = gExpLevelUp + gLEVEL_UP_AT
                    gPlayerMaxHP = gPlayerMaxHP + gSTART_HIT_POINTS
                    AddMessage("Welcome to level " + CStr(gPlayerLvl))
                    If (gPlayerLvl = 10) Then
                        gPlaying = False
                        ' Make sure the win message does not scroll off to the right
                        InitMessage()
                        AddMessage("You win.")
                    End If
                End If
            End If
        End If
    End Sub

    Private Sub ProcessPlayer(ByVal nX As Integer, ByVal nY As Integer)
        Dim bStep As Boolean
        bStep = False
        If CanMove(nX, nY) Then
            ErasePlayer()
            gPlayerX = nX
            gPlayerY = nY
            If (gArrMaze(gPlayerY, gPlayerX) = gPELLET) Then
                gCountDown = gGHOST_RUN_DURATION
                RedrawGhosts()
            End If
            gArrMaze(gPlayerY, gPlayerX) = gPLAYER
            bStep = True
        ElseIf IsGhost(nX, nY) Then
            HitGhost(nX, nY)
            bStep = True
        End If
        If bStep Then
            If (gCountDown > 0) Then
                gCountDown = gCountDown - 1
                If (gCountDown = 0) Then
                    RedrawGhosts()
                End If
            End If
            Regenerate()
            ProcessGhosts()
            Me.Refresh()
            InitMessage()
        End If
    End Sub

    Private Sub Form1_KeyPress(ByVal sender As System.Object, ByVal e As System.Windows.Forms.KeyPressEventArgs) Handles MyBase.KeyPress
        Dim bAction As Boolean
        bAction = False
        Dim nNewX As Integer
        Dim nNewY As Integer
        nNewX = gPlayerX
        nNewY = gPlayerY
        If (e.KeyChar = "8") Then
            nNewY = nNewY - 1
            bAction = True
        ElseIf (e.KeyChar = "9") Then
            nNewY = nNewY - 1
            nNewX = nNewX + 1
            bAction = True
        ElseIf (e.KeyChar = "6") Then
            nNewX = nNewX + 1
            bAction = True
        ElseIf (e.KeyChar = "3") Then
            nNewY = nNewY + 1
            nNewX = nNewX + 1
            bAction = True
        ElseIf (e.KeyChar = "2") Then
            nNewY = nNewY + 1
            bAction = True
        ElseIf (e.KeyChar = "1") Then
            nNewY = nNewY + 1
            nNewX = nNewX - 1
            bAction = True
        ElseIf (e.KeyChar = "4") Then
            nNewX = nNewX - 1
            bAction = True
        ElseIf (e.KeyChar = "7") Then
            nNewY = nNewY - 1
            nNewX = nNewX - 1
            bAction = True
        End If
        If gPlaying And bAction Then
            ProcessPlayer(nNewX, nNewY)
        End If
    End Sub
End Class
