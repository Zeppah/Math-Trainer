Public Class Form1
    Public Difficulty As Int16 = 10
    Public Operation As String = "+"
    Public AnswerOperation As String
    Public Streak As Int32, NumberOfClicks As Int32
    Public Num1 As Int16
    Public Num2 As Int16
    Public Answer As Int16
    Public DataPath As String = Application.UserAppDataPath

    Private Sub Form1_Load(sender As Object, e As EventArgs) Handles MyBase.Load
        Me.CenterToScreen()
        pnlClicker.Hide()
        pnlFortune.Hide()
        If My.Computer.FileSystem.FileExists(DataPath + "\settings.txt") Then
            ReadSettingsFile()
            lblHighStreak.Text = Streak
            lblClickCount.Text = NumberOfClicks
        Else
            Streak = 0
            NumberOfClicks = 0
            WriteSettingsFile()
        End If

    End Sub

    'Check Answer Sub
    Private Sub btnEnter_Click(sender As Object, e As EventArgs) Handles btnEnter.Click
        Dim Button As Button = sender
        If btnClicker.Tag = "Y" Then
            Return
        End If
        If Button.Tag = 6 Then
            txtAnswer.Text = ""
            lblEquation.Text = ""
            lblInstructions.Text = ""
            Button.Tag = 1
            GenerateQuestion()
        Else
            CheckAnswer()
        End If
    End Sub

    'Generate a new equation
    Private Sub GenerateQuestion()
        Dim tempnum As Int16
        Randomize()
        Num1 = Int((Difficulty * Rnd()))
        Num2 = Int((Difficulty * Rnd()))

        If Operation = "R" Then
            tempnum = Int(5 * Rnd())
            Select Case tempnum
                Case 0
                    AnswerOperation = "+"
                Case 1

                    AnswerOperation = "-"
                Case 2
                    AnswerOperation = "x"
                Case 3
                    AnswerOperation = "/"
                Case Else
            End Select
        Else
            AnswerOperation = Operation
        End If
        Select Case AnswerOperation
            Case "-"
                If Num1 < Num2 Then
                    tempnum = Num1
                    Num1 = Num2
                    Num2 = tempnum
                End If
                lblInstructions.Text = "Subtraction"
                Answer = Num1 - Num2
            Case "/"
                If Num1 = 0 Then
                    Num1 = 1
                End If
                If Num2 = 0 Then
                    Num2 = 1
                End If
                tempnum = Num1 * Num2
                Num1 = tempnum
                lblInstructions.Text = "Division"
                Answer = Num1 / Num2
            Case "x"
                lblInstructions.Text = "Multiplication"
                Answer = Num1 * Num2
            Case "+"
                lblInstructions.Text = "Addition"
                Answer = Num1 + Num2
        End Select
        lblEquation.Text = Num1 & " " & AnswerOperation & " " & Num2 & " = ?"

    End Sub

    'Check the Answer
    Private Sub CheckAnswer()
        If txtAnswer.Text = "" Then
            Return
        End If
        If Val(txtAnswer.Text) = Answer Then
            lblInstructions.Text = Num1 & " " & AnswerOperation & " " & Num2 & " = " & Answer & vbCrLf & "was Correct"
            lblEquation.Text = "Press Enter for next equation"
            txtAnswer.Text = ""
            btnEnter.Tag = 6
            My.Computer.Audio.PlaySystemSound(System.Media.SystemSounds.Exclamation)
            lblCurrentStreak.Text = Val(lblCurrentStreak.Text) + 1
            If Val(lblCurrentStreak.Text) > Val(lblHighStreak.Text) Then
                lblHighStreak.Text = Val(lblHighStreak.Text) + 1
                Streak = Val(lblHighStreak.Text)
                WriteSettingsFile()
            End If
            lblCorrect.Text = Val(lblCorrect.Text) + 1
            Select Case AnswerOperation
                Case "-"
                    lblCSub.Text = Val(lblCSub.Text) + 1
                Case "/"
                    lblCDiv.Text = Val(lblCDiv.Text) + 1
                Case "x"
                    lblCMult.Text = Val(lblCMult.Text) + 1
                Case "+"
                    lblCAdd.Text = Val(lblCAdd.Text) + 1
            End Select
        Else
            lblInstructions.Text = "Incorrect"
            My.Computer.Audio.PlaySystemSound(System.Media.SystemSounds.Hand)
            lblCurrentStreak.Text = 0
            lblInCorrect.Text = Val(lblInCorrect.Text) + 1
            txtAnswer.Text = ""
            Select Case AnswerOperation
                Case "-"
                    lblWSub.Text = Val(lblWSub.Text) + 1
                Case "/"
                    lblWDiv.Text = Val(lblWDiv.Text) + 1
                Case "x"
                    lblWMult.Text = Val(lblWMult.Text) + 1
                Case "+"
                    lblWAdd.Text = Val(lblWAdd.Text) + 1
            End Select
        End If
    End Sub

    'Display Number
    Private Sub NumberEntered_Click(sender As Object, e As EventArgs) Handles btn1.Click, btn9.Click, btn8.Click, btn7.Click, btn6.Click, btn5.Click, btn4.Click, btn3.Click, btn2.Click, btn0.Click, btnClr.Click, btnBS.Click
        Dim Button As Button = sender

        If Button.Tag = "Clear" Then
            txtAnswer.Text = ""
            Return
        End If

        If Button.Tag = "BS" Then
            If Len(txtAnswer.Text) > 0 Then
                txtAnswer.Text = Microsoft.VisualBasic.Left(txtAnswer.Text, Len(txtAnswer.Text) - 1)
            End If
            Return
        End If
        txtAnswer.Text = txtAnswer.Text + Button.Tag
    End Sub

    'Play the clicker game
    Private Sub btnClicker_Click(sender As Object, e As EventArgs) Handles btnClicker.Click, btnExit.Click, btnClickMe.Click
        Dim Button As Button = sender
        lblClickCount.Text = NumberOfClicks
        Select Case Button.Text
            Case "Take a break!"
                lblInstructions.Text = "Fortune? or ..." & vbCrLf & "Build Lifetime clicks?"
                lblEquation.Text = "Play Time!"
                txtAnswer.Text = ""
                btnClicker.Tag = "Y"
                pnlClicker.Show()
                pnlFortune.Show()
            Case "Click Me!"
                lblClickCount.Focus()
                lblClickCount.Text = Val(lblClickCount.Text) + 1
                NumberOfClicks = Val(lblClickCount.Text)
            Case "Exit"
                btnClicker.Tag = "N"
                '               WriteSettingsFile()
                pnlClicker.Hide()
                pnlFortune.Hide()
                lblEquation.Text = "Play Time, over" & vbCrLf & "back to work."
                lblInstructions.Text = "Press Enter" & vbCrLf & "to begin."
                txtAnswer.Text = ""
                btnEnter.Tag = 6
            Case Else

        End Select
    End Sub

    'Tell the Fortune
    Sub AskFortune_Click(sender As Object, e As EventArgs) Handles btnFortune.Click
        Dim tempnum As Int16
        Randomize()
        tempnum = Int(20 * Rnd())
        Select Case tempnum
            Case 1
                lblInstructions.ForeColor = Color.LightGreen
                lblInstructions.Text = "It is certain."
            Case 2
                lblInstructions.ForeColor = Color.LightGreen
                lblInstructions.Text = "It is decidedly so."
            Case 3
                lblInstructions.ForeColor = Color.LightGreen
                lblInstructions.Text = "Without a doubt."
            Case 4
                lblInstructions.ForeColor = Color.LightGreen
                lblInstructions.Text = "Yes - definately."
            Case 5
                lblInstructions.ForeColor = Color.LightGreen
                lblInstructions.Text = "You may rely on it."
            Case 6
                lblInstructions.ForeColor = Color.LightGreen
                lblInstructions.Text = "As I see it, yes."
            Case 7
                lblInstructions.ForeColor = Color.LightGreen
                lblInstructions.Text = "Most likely."
            Case 8
                lblInstructions.ForeColor = Color.LightGreen
                lblInstructions.Text = "Outlook good."
            Case 9
                lblInstructions.ForeColor = Color.LightGreen
                lblInstructions.Text = "Yes."
            Case 10
                lblInstructions.ForeColor = Color.LightGreen
                lblInstructions.Text = "Signs point to yes."
            Case 11
                lblInstructions.ForeColor = Color.Yellow
                lblInstructions.Text = "Reply hazy, try again."
            Case 12
                lblInstructions.ForeColor = Color.Yellow
                lblInstructions.Text = "Ask again later."
            Case 13
                lblInstructions.ForeColor = Color.Yellow
                lblInstructions.Text = "Better not tell you now."
            Case 14
                lblInstructions.ForeColor = Color.Yellow
                lblInstructions.Text = "Cannot predict now."
            Case 15
                lblInstructions.ForeColor = Color.Yellow
                lblInstructions.Text = "Concentrate and ask again."
            Case 16
                lblInstructions.ForeColor = Color.Red
                lblInstructions.Text = "Don't count on it."
            Case 17
                lblInstructions.ForeColor = Color.Red
                lblInstructions.Text = "My reply is no."
            Case 18
                lblInstructions.ForeColor = Color.Red
                lblInstructions.Text = "My sources say no."
            Case 19
                lblInstructions.ForeColor = Color.Red
                lblInstructions.Text = "Outlook not so good."
            Case 20
                lblInstructions.ForeColor = Color.Red
                lblInstructions.Text = "Very doubtful."
        End Select
    End Sub
    'Display the Hint tables
    Private Sub btnMultTable_Click(sender As Object, e As EventArgs) Handles btnMultTable.Click
        If btnClicker.Tag = "Y" Then
            Return
        End If
        If AnswerOperation = "x" Or AnswerOperation = "/" Then
            Form2.Show()
        Else
            Form3.Show()
        End If
    End Sub

    'Set the difficulty Level
    Private Sub btnDifficulty_Click(sender As Object, e As EventArgs) Handles btnEasy.Click, btnMedium.Click, btnHard.Click
        Dim Button As Button = sender
        If btnClicker.Tag = "Y" Then
            Return
        End If
        Difficulty = Val(Button.Tag)
        btnEasy.BackColor = Color.Gainsboro
        btnMedium.BackColor = Color.Gainsboro
        btnHard.BackColor = Color.Gainsboro
        Button.BackColor = Color.Gray
    End Sub

    'Set the Operation
    Private Sub btnOperation_Click(sender As Object, e As EventArgs) Handles btnRandom.Click, btnSubtract.Click, btnMult.Click, btnDivide.Click, btnAdd.Click
        Dim Button As Button = sender
        If btnClicker.Tag = "Y" Then
            Return
        End If
        btnRandom.BackColor = Color.Gainsboro
        btnMult.BackColor = Color.Gainsboro
        btnDivide.BackColor = Color.Gainsboro
        btnAdd.BackColor = Color.Gainsboro
        btnSubtract.BackColor = Color.Gainsboro
        Button.BackColor = Color.Gray
        Operation = Button.Tag
    End Sub

    'Save High Streak and Lifetime Clicks
    Sub WriteSettingsFile()
        Dim FileString As String
        FileString = Streak & "," & NumberOfClicks
        My.Computer.FileSystem.WriteAllText(DataPath + "\settings.txt", FileString, False)
    End Sub

    'Load Streak and Lifetime Clicks
    Sub ReadSettingsFile()
        Using MyReader As New FileIO.TextFieldParser(DataPath + "\settings.txt")
            MyReader.TextFieldType = FileIO.FieldType.Delimited
            MyReader.SetDelimiters(",")
            Dim MyData() As String = MyReader.ReadFields()
            Streak = Val(MyData(0))
            NumberOfClicks = Val(MyData(1))
        End Using
    End Sub
End Class
