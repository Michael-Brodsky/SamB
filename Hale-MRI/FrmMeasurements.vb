Imports LibDatabase.Contexts
Imports LibDatabase.Models
Imports LibDatabase.Imex
Imports LibEncoder
Imports LibEncoder.USDigital
Imports Microsoft.EntityFrameworkCore
Imports System.Collections.Immutable
Public Class FrmMeasurements
    Inherits FrmDatabaseForm
#Region "Constants"

#End Region
#Region "Private Members"
    Private mBlades As Integer
    Private mHardware As WorkstationEncoders
    Private mJobDetails As JobDetail
    Private mJob As Job
    Private Scanning As Boolean
#End Region
#Region "Public Interface"
    Public Property Hardware As WorkstationEncoders
        Get
            Return mHardware
        End Get
        Set(value As WorkstationEncoders)
            mHardware = value
        End Set
    End Property
    Public Property Job As Job
        Get
            Return mJob
        End Get
        Set(value As Job)
            mJob = value
            If mJob IsNot Nothing Then
                For i As Integer = 1 To mJob.Blades
                    ComboBladeId.Items.Add(i.ToString())
                Next
            End If
        End Set
    End Property
    Public Property JobDetails As JobDetail
        Get
            Return mJobDetails
        End Get
        Set(value As JobDetail)
            If value IsNot Nothing Then
                mJobDetails = value
                If mJobDetails IsNot Nothing Then
                    RadiusMeasurementBindingSource.DataSource = Database.RadiusMeasurements.Where(Function(j) j.JobDetailsId = mJobDetails.Id).ToList()
                    CellMeasurementsBindingSource.DataSource = Database.CellMeasurements.Where(Function(j) j.JobDetailsId = mJobDetails.Id).ToList()
                    ExtremeMeasurementsBindingSource.DataSource = Database.ExtremeMeasurements.Where(Function(j) j.JobDetailsId = mJobDetails.Id).ToList()
                End If
            End If
        End Set
    End Property
#End Region
#Region "Private Interface"

    Private Sub Scan(ScanRadius As Double, ScanBlade As Integer, ScanAngle As Double)
        If Database IsNot Nothing Then
            Dim BladeIDs As New List(Of Integer?)
            Dim Radii As New List(Of Double?)
            Dim LECells As New List(Of Integer?)
            Dim TECells As New List(Of Integer?)
            Dim Angles As New List(Of Double?)
            Dim Depths As New List(Of Double?)
            If Database IsNot Nothing Then
                ' Get the existing radius measurements for the current job details
                For Each bladID In Database.RadiusMeasurements.Where(Function(r) r.JobDetailsId = JobDetails.Id).AsSplitQuery().Select(Function(r) r.BladeId).ToList()
                    BladeIDs.Add(bladID)
                Next
                For Each Rad In Database.RadiusMeasurements.Where(Function(r) r.JobDetailsId = JobDetails.Id).AsSplitQuery().Select(Function(r) r.Radius).ToList()
                    Radii.Add(Rad)
                Next
                For Each integ In Database.RadiusMeasurements.Where(Function(r) r.JobDetailsId = JobDetails.Id).AsSplitQuery().Select(Function(r) r.LeCell).ToList()
                    LECells.Add(integ)
                Next
                For Each integ In Database.RadiusMeasurements.Where(Function(r) r.JobDetailsId = JobDetails.Id).AsSplitQuery().Select(Function(r) r.TeCell).ToList()
                    TECells.Add(integ)
                Next
                For Each Ange In Database.CellMeasurements.Where(Function(c) c.JobDetailsId = JobDetails.Id).AsSplitQuery().Select(Function(c) c.Angle).ToList()
                    Angles.Add(Ange)
                Next
                For Each Dept In Database.CellMeasurements.Where(Function(c) c.JobDetailsId = JobDetails.Id).AsSplitQuery().Select(Function(c) c.Depth).ToList()
                    Depths.Add(Dept)
                Next
            End If
            Dim AngleArray As Double() = New Double(0) {}
            Dim DepthArray As Double() = New Double(0) {}
            Dim n As Integer = 1
            Dim pointtotal As Integer = (360 / Job.Blades)
            Dim ScanIncrement As Double = 360 * Hardware.Workstation.ScanIncrement / Hardware.Workstation.AngleResolution 'This is the increment in degrees for each scan point
            TxtStatus.Text = "Scanning Blade " & ScanBlade.ToString() & " at " & ScanRadius.ToString() & "% Radius"
            If ScanRadius < 0 Or ScanRadius > 100 Then
                MessageBox.Show("Invalid radius value. Please enter a value between 0 and 100.", "Invalid Radius", MessageBoxButtons.OK, MessageBoxIcon.Error)
                TxtStatus.Text = "Idle"
                Return
            End If
            With Hardware.Encoders
                AngleArray(0) = .Angle()
                DepthArray(0) = .Depth()
                For n = 1 To pointtotal
                    While .Angle() > Int((AngleArray(n - 1) + ScanIncrement) / ScanIncrement + 0.5) / ScanIncrement
                        If Scanning = False Then
                            GoTo exittheFor
                        End If
                        .Angle()
                        .Depth()
                        System.Threading.Thread.Sleep(5)
                    End While

                    If .Angle() < 180 And ScanBlade = 1 Then
                        AngleArray(n) = .Angle() - 360
                    Else
                        AngleArray(n) = .Angle()
                    End If
                    DepthArray(n) = .Depth()
                Next
exittheFor:
                If .Angle() < 180 And ScanBlade = 1 Then
                    AngleArray(n) = .Angle() - 360
                Else
                    AngleArray(n) = .Angle()
                End If
                DepthArray(n) = .Depth()
                timerMeasurements.Enabled = True
                'Need to add a check for duplicate radius measurements for the same blade and radius so we can remove old data
                ' Save the measurements to the database
                Dim needdelete As Boolean = False
                Dim celltotal As Integer = 0
                Dim x As Integer = 0
                For Each bladID In BladeIDs
                    If bladID.Value = ScanBlade And Math.Round(Radii(x).Value) = Math.Round(ScanRadius) Then
                        needdelete = True
                        Exit For
                    End If
                    Dim lecell As Integer = LECells(x).Value
                    Dim tecell As Integer = TECells(x).Value
                    celltotal += tecell - lecell + 1 ' + 1 to include the cell stated by the actual values
                    x += 1
                Next


            End With
        End If
    End Sub
    Private ReadOnly Property PitchofRadiusSegments As Double()
        Get
            ' This property calculates the average pitch for each radius segment based on the radius measurements and cell measurements. It returns an array of average Pitch Values
            Dim pitcharray As Double()
            If Database IsNot Nothing Then

                Dim BladeIDs As New List(Of Integer?)
                Dim Radii As New List(Of Double?)
                Dim LECells As New List(Of Integer?)
                Dim TECells As New List(Of Integer?)
                Dim Angles As New List(Of Double?)
                Dim Depths As New List(Of Double?)
                For Each bladID In Database.RadiusMeasurements.Where(Function(r) r.JobDetailsId = JobDetails.Id).AsSplitQuery().Select(Function(r) r.BladeId).ToList()
                    BladeIDs.Add(bladID)
                Next
                For Each Rad In Database.RadiusMeasurements.Where(Function(r) r.JobDetailsId = JobDetails.Id).AsSplitQuery().Select(Function(r) r.Radius).ToList()
                    Radii.Add(Rad)
                Next
                For Each integ In Database.RadiusMeasurements.Where(Function(r) r.JobDetailsId = JobDetails.Id).AsSplitQuery().Select(Function(r) r.LeCell).ToList()
                    LECells.Add(integ)
                Next
                For Each integ In Database.RadiusMeasurements.Where(Function(r) r.JobDetailsId = JobDetails.Id).AsSplitQuery().Select(Function(r) r.TeCell).ToList()
                    TECells.Add(integ)
                Next
                For Each Ange In Database.CellMeasurements.Where(Function(c) c.JobDetailsId = JobDetails.Id).AsSplitQuery().Select(Function(c) c.Angle).ToList()
                    Angles.Add(Ange)
                Next
                For Each Dept In Database.CellMeasurements.Where(Function(c) c.JobDetailsId = JobDetails.Id).AsSplitQuery().Select(Function(c) c.Depth).ToList()
                    Depths.Add(Dept)
                Next
                Dim PitchbyBladeRadius = New Double(BladeIDs.Count) {}

                Dim cellcount As Integer = 0
                For i = 0 To BladeIDs.Count - 1
                    Dim bladid = BladeIDs(i)
                    Dim bladeIndex As Integer = bladid - 1
                    Dim radius As Double = Radii(i).GetValueOrDefault()
                    Dim leCell As Integer = LECells(i).GetValueOrDefault()
                    Dim teCell As Integer = TECells(i).GetValueOrDefault()
                    Dim celldiff As Integer = teCell - leCell + cellcount

                    Dim totalpitch As Double = 0
                    Dim pitchcount As Integer = 0
                    For x = cellcount To celldiff - 1
                        Dim angle1 As Double = Angles(x).GetValueOrDefault()
                        Dim depth1 As Double = Depths(x).GetValueOrDefault()
                        Dim angle2 As Double = Angles(x + 1).GetValueOrDefault()
                        Dim depth2 As Double = Depths(x + 1).GetValueOrDefault()
                        Dim pitch As Double = MRIMath.GetPitch(angle1, angle2, depth1, depth2)
                        totalpitch += pitch
                        pitchcount += 1
                    Next
                    If pitchcount > 0 Then
                        Dim averagePitch As Double = Math.Round(totalpitch / pitchcount, 2)
                        PitchbyBladeRadius(i) = averagePitch
                    End If
                Next
                pitcharray = PitchbyBladeRadius
            End If

            Return pitcharray
        End Get
    End Property
    Private Sub HomeEncoders()
        With Hardware.Encoders
            .ResetCount(USDigital.ANGLE_ENCODER)
            .ResetCount(USDigital.RADIUS_ENCODER)
            .ResetCount(USDigital.DEPTH_ENCODER)
        End With
        cmdHome.Visible = False
        cmdHome.Enabled = False
    End Sub
    Private Sub MeasurementsGet()
        ' Uset this in place of UpdateFields()
        With Hardware.Encoders
            txtAngle.Text = .Angle
            txtRadius.Text = .Radius(JobDetails.Diameter).Value
            txtDepth.Text = .Depth
            txtRadiusPercent.Text = .Radius((JobDetails.Diameter).Value * 100.0).ToString()
        End With
    End Sub
    Private Sub MeasurementsSave()
        ' Saves the currently displayed measurements to the database.
        If Database IsNot Nothing Then
            Dim cm As New CellMeasurement With {
                .JobDetailsId = mJobDetails.Id,
                .Angle = Convert.ToDouble(txtAngle.Text),
                .Depth = Convert.ToDouble(txtDepth.Text)
            }
            Dim rm As New RadiusMeasurement With {
                .JobDetailsId = mJobDetails.Id,
                .Radius = Convert.ToDouble(txtRadius.Text),
                .BladeId = ComboBladeId.SelectedIndex + 1
            }
            Database.CellMeasurements.Add(cm)
            Database.RadiusMeasurements.Add(rm)
            Database.SaveChanges()
        End If
    End Sub
    Private Sub UpdateFields()
        With Hardware.Encoders
            txtAngle.Text = .Angle
            txtRadius.Text = .Radius(mJobDetails.Diameter).Value
            txtDepth.Text = .Depth
            txtRadiusPercent.Text = .Radius((mJobDetails.Diameter).Value * 100.0).ToString()
        End With
    End Sub
    Private Sub UpdatePitchByRadiusTableFull()
        'need to implement a method to check the table for existing data and clear or update if necessary
        While GridBladebyRadius.Columns.GetColumnCount(DataGridViewElementStates.Visible) > 1
            GridBladebyRadius.Columns.RemoveAt(GridBladebyRadius.Columns.Count - 1) 'remove all but the blade column
        End While
        GridBladebyRadius.Rows.Clear() 'remove all rows
        If Database IsNot Nothing Then
            Dim BladeIDs As New List(Of Integer?) 'Commented out sections have been moved to the PitchofRadiusSegments property as it will be used in multiple places
            Dim Radii As New List(Of Double?)
            'Dim LECells As New List(Of Integer?)
            'Dim TECells As New List(Of Integer?)
            'Dim Angles As New List(Of Double?)
            'Dim Depths As New List(Of Double?)

            'Database.RadiusMeasurements.OrderBy(Of Integer)(Function(r) r.BladeId).Where(Function(r) r.JobDetailsId = JobDetails.Id).AsSplitQuery().Where(Function(r) r.BladeId).ToList()

            Dim colBladeIDS As New List(Of Integer?)
            Dim colRadii As New List(Of Double?)
            For Each bladID In Database.RadiusMeasurements.Where(Function(r) r.JobDetailsId = JobDetails.Id).AsSplitQuery().Select(Function(r) r.BladeId).ToList()
                BladeIDs.Add(bladID)
            Next
            For Each Rad In Database.RadiusMeasurements.Where(Function(r) r.JobDetailsId = JobDetails.Id).AsSplitQuery().Select(Function(r) r.Radius).ToList()
                Radii.Add(Rad)
            Next
            For Each BladID In Database.RadiusMeasurements.Where(Function(r) r.JobDetailsId = JobDetails.Id).OrderBy(Function(r) r.Radius).Distinct.ToList()
                colBladeIDS.Add(BladID.BladeId)
                colRadii.Add(BladID.Radius)
            Next


            'For Each integ In Database.RadiusMeasurements.Where(Function(r) r.JobDetailsId = JobDetails.Id).AsSplitQuery().Select(Function(r) r.LeCell).ToList()
            '    LECells.Add(integ)
            'Next
            'For Each integ In Database.RadiusMeasurements.Where(Function(r) r.JobDetailsId = JobDetails.Id).AsSplitQuery().Select(Function(r) r.TeCell).ToList()
            '    TECells.Add(integ)
            'Next
            'For Each Ange In Database.CellMeasurements.Where(Function(c) c.JobDetailsId = JobDetails.Id).AsSplitQuery().Select(Function(c) c.Angle).ToList()
            '    Angles.Add(Ange)
            'Next
            'For Each Dept In Database.CellMeasurements.Where(Function(c) c.JobDetailsId = JobDetails.Id).AsSplitQuery().Select(Function(c) c.Depth).ToList()
            '    Depths.Add(Dept)
            'Next

            'Dim cellcount As Integer = 0
            'For i = 0 To BladeIDs.Count - 1
            '    Dim bladid = BladeIDs(i)
            '    Dim bladeIndex As Integer = bladid - 1
            '    Dim radius As Double = Radii(i).GetValueOrDefault()
            '    Dim leCell As Integer = LECells(i).GetValueOrDefault()
            '    Dim teCell As Integer = TECells(i).GetValueOrDefault()
            '    Dim celldiff As Integer = teCell - leCell + cellcount

            '    Dim totalpitch As Double = 0
            '    Dim pitchcount As Integer = 0
            '    For x = cellcount To celldiff - 1
            '        Dim angle1 As Double = Angles(x).GetValueOrDefault()
            '        Dim depth1 As Double = Depths(x).GetValueOrDefault()
            '        Dim angle2 As Double = Angles(x + 1).GetValueOrDefault()
            '        Dim depth2 As Double = Depths(x + 1).GetValueOrDefault()
            '        Dim pitch As Double = MRIMath.GetPitch(angle1, angle2, depth1, depth2)
            '        totalpitch += pitch
            '        pitchcount += 1
            '    Next
            '    If pitchcount > 0 Then
            '        Dim averagePitch As Double = Math.Round(totalpitch / pitchcount, 2)
            '        PitchbyBladeRadius(i) = averagePitch
            '    End If
            'Next

            Dim PitchbyBladeRadius As Double() = PitchofRadiusSegments
            Dim radcountlist = colBladeIDS.FindAll(Function(b) b = 1)
            Dim Raditerator = 0

            While GridBladebyRadius.Columns.GetColumnCount(DataGridViewElementStates.Visible) - 1 < radcountlist.Count
                Dim Radius As Double = colRadii(Raditerator).Value
                Radius = Math.Round(Radius, 0)
                GridBladebyRadius.Columns.Add(Radius.ToString(), Radius & "%")
                Raditerator += 1
            End While

            While GridBladebyRadius.Rows.Count < Job.Blades
                GridBladebyRadius.Rows.Add()
            End While
            For Each row In GridBladebyRadius.Rows
                GridBladebyRadius.Rows(row.Index).Cells(0).Value = row.Index + 1
            Next
            Raditerator = 0
            For i = 0 To BladeIDs.Count - 1
                Dim Bindex As Integer = BladeIDs(i) - 1
                GridBladebyRadius.Rows(Bindex).Cells(Math.Round(Radii(i).Value, 0).ToString()).Value = PitchbyBladeRadius(i)
                Raditerator += 1
            Next
            GridBladebyRadius.AutoResizeColumns(DataGridViewAutoSizeColumnsMode.ColumnHeader)

        End If
    End Sub

#End Region
#Region "UI Event Handlers"
    Private Sub CmdHome_Click(sender As Object, e As EventArgs) Handles cmdHome.Click
        HomeEncoders()
    End Sub
    Private Sub CmdStopScan_Click(sender As Object, e As EventArgs)
        Try
            MeasurementsSave()
        Catch ex As Exception
            MessageBox.Show("Error saving measurements: " & ex.Message, STR_TITLE_APPLICATION_ERROR, MessageBoxButtons.OK, MessageBoxIcon.Error)
        End Try
    End Sub
    Private Sub CountUpdate_Tick(sender As Object, e As EventArgs)
        'UpdateFields()
    End Sub
    Private Sub CmdZero_Click(sender As Object, e As EventArgs) Handles cmdZero.Click
        Try
            mHardware.Encoders.ResetCount(ANGLE_ENCODER)
            mHardware.Encoders.ResetCount(RADIUS_ENCODER)
            mHardware.Encoders.ResetCount(DEPTH_ENCODER)
        Catch ex As Exception
            MessageBox.Show("Error zeroing encoders: " & ex.Message, STR_TITLE_APPLICATION_ERROR, MessageBoxButtons.OK, MessageBoxIcon.Error)
        End Try
    End Sub

    Private Sub CmdStartScan_Click(sender As Object, e As EventArgs)
        Try
            MeasurementsGet()
        Catch ex As Exception
            MessageBox.Show("Error getting measurements: " & ex.Message, STR_TITLE_APPLICATION_ERROR, MessageBoxButtons.OK, MessageBoxIcon.Error)
        End Try
    End Sub

    Private Sub FrmMeasurements_Load(sender As Object, e As EventArgs) Handles MyBase.Load
        Try
            Job = Database.Jobs.Where(Function(j) j.JobNumber = 3427).FirstOrDefault()
            JobDetails = Database.JobDetails.FirstOrDefault(Function(j) j.JobId = Job.Id)
            timerMeasurements.Interval = 100 'Database.Settings.FirstOrDefault().EncoderCalibrationSampleRate
        Catch ex As Exception
            MessageBox.Show("Error loading settings: " & ex.Message, STR_TITLE_APPLICATION_ERROR, MessageBoxButtons.OK, MessageBoxIcon.Error)
        End Try
    End Sub

    Private Sub ChkMeasurements_CheckedChanged(sender As Object, e As EventArgs) Handles chkMeasurements.CheckedChanged
        Try
            timerMeasurements.Enabled = chkMeasurements.Checked
            cmdHome.Enabled = Not chkMeasurements.Checked
        Catch ex As Exception
            MsgBox(ex.Message, MsgBoxStyle.Critical, STR_TITLE_APPLICATION_ERROR)
        End Try
        Try
            UpdatePitchByRadiusTableFull()
        Catch ex As Exception
            MsgBox(ex.Message, MsgBoxStyle.Critical, STR_TITLE_APPLICATION_ERROR)
        End Try
    End Sub

    Private Sub TimerMeasurements_Tick(sender As Object, e As EventArgs) Handles timerMeasurements.Tick
        Try
            MeasurementsGet()
        Catch ex As Exception
            chkMeasurements.Checked = False
            MessageBox.Show("Error updating measurements: " & ex.Message, STR_TITLE_APPLICATION_ERROR, MessageBoxButtons.OK, MessageBoxIcon.Error)
        End Try
    End Sub
#End Region
End Class