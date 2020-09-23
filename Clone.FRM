VERSION 5.00
Begin VB.Form Form1 
   AutoRedraw      =   -1  'True
   BorderStyle     =   0  'None
   Caption         =   "Form1"
   ClientHeight    =   3195
   ClientLeft      =   0
   ClientTop       =   0
   ClientWidth     =   4680
   LinkTopic       =   "Form1"
   MousePointer    =   99  'Custom
   ScaleHeight     =   3195
   ScaleWidth      =   4680
   ShowInTaskbar   =   0   'False
   StartUpPosition =   3  'Windows Default
End
Attribute VB_Name = "Form1"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Dim Cycle As Long
Dim Cycle2 As Long
Dim New_Vertex As Long
Dim New_Vertex2 As Long
Dim nexte As Integer
Dim nexte2 As Integer
' Check Keyboard
Dim view    As Long
Dim col     As Boolean
Dim key_p   As Boolean
Dim up_k    As Boolean
Dim down_k  As Boolean
Dim up_p    As Boolean
Dim down_p  As Boolean
' Check Positions
Dim pos    As D3DVECTOR
Dim pos2   As D3DVECTOR
Dim pos3   As D3DVECTOR
Dim pos4   As D3DVECTOR
' Load Objects
Dim Frame_Main_Car   As Direct3DRMFrame3
Dim Object_Main_Car  As Direct3DRMMeshBuilder3
Dim Frame_Car        As Direct3DRMFrame3
Dim Object_Car       As Direct3DRMMeshBuilder3
Dim Frame_World      As Direct3DRMFrame3
Dim Object_World     As Direct3DRMMeshBuilder3
' Init DirectX7
Dim DX_Main As New DirectX7
Dim DD_Main As DirectDraw4
Dim D3D_Main As Direct3DRM3
Dim DI_Main As DirectInput
Dim DI_Device As DirectInputDevice
Dim DI_State As DIKEYBOARDSTATE
Dim DS_Front As DirectDrawSurface4
Dim DS_Back As DirectDrawSurface4
Dim SD_Front As DDSURFACEDESC2
Dim DD_Back As DDSCAPS2
Dim D3D_Device As Direct3DRMDevice3
Dim D3D_ViewPort As Direct3DRMViewport2
Dim FR_Root As Direct3DRMFrame3
Dim Main_Car_Root As Direct3DRMFrame3
Dim Car_Root As Direct3DRMFrame3
Dim World_Root As Direct3DRMFrame3
Dim FR_Camera As Direct3DRMFrame3
Dim FR_Light As Direct3DRMFrame3
Dim LT_Ambient As Direct3DRMLight
Dim LT_Spot As Direct3DRMLight
Dim esc As Boolean
Dim BackGround As Direct3DRMTexture3
' Init Music
Private DS_Main       As DirectSound
Private DS_BGMusic    As DirectSoundBuffer
Private DS_StartSound As DirectSoundBuffer

Private Sub DX_Init()
Dim SoundDesc As DSBUFFERDESC, WavFormat As WAVEFORMATEX
Set DS_Main = DX_Main.DirectSoundCreate("")
    DS_Main.SetCooperativeLevel Me.hWnd, DSSCL_NORMAL
 
 Set DD_Main = DX_Main.DirectDraw4Create("")
    DD_Main.SetCooperativeLevel Form1.hWnd, DDSCL_FULLSCREEN Or DDSCL_EXCLUSIVE
    DD_Main.SetDisplayMode 640, 480, 32, 0, DDSDM_DEFAULT
     SD_Front.lFlags = DDSD_CAPS Or DDSD_BACKBUFFERCOUNT
    SD_Front.ddsCaps.lCaps = DDSCAPS_PRIMARYSURFACE Or DDSCAPS_3DDEVICE Or DDSCAPS_COMPLEX Or _
    DDSCAPS_FLIP
    SD_Front.lBackBufferCount = 1
 Set DS_Front = DD_Main.CreateSurface(SD_Front)
     DD_Back.lCaps = DDSCAPS_BACKBUFFER
 Set DS_Back = DS_Front.GetAttachedSurface(DD_Back)
     DS_Back.SetForeColor RGB(255, 255, 255)
 Set D3D_Main = DX_Main.Direct3DRMCreate()

 Set D3D_Device = D3D_Main.CreateDeviceFromSurface("IID_IDirect3DHALDevice", DD_Main, DS_Back, _
    D3DRMDEVICE_DEFAULT)
    D3D_Device.SetBufferCount 2
    D3D_Device.SetQuality D3DRMRENDER_GOURAUD
    D3D_Device.SetTextureQuality D3DRMTEXTURE_NEAREST
    D3D_Device.SetRenderMode D3DRMRENDERMODE_BLENDEDTRANSPARENCY

 Set DI_Main = DX_Main.DirectInputCreate()
 Set DI_Device = DI_Main.CreateDevice("GUID_SysKeyboard")
    DI_Device.SetCommonDataFormat DIFORMAT_KEYBOARD
    DI_Device.SetCooperativeLevel Me.hWnd, DISCL_BACKGROUND Or DISCL_NONEXCLUSIVE
    DI_Device.Acquire
 Set DS_BGMusic = DS_Main.CreateSoundBufferFromFile(App.Path & "\backgroundmusic.wav", SoundDesc, WavFormat)
End Sub

Private Sub DX_MakeObjects()
 Set FR_Root = D3D_Main.CreateFrame(Nothing)
 Set FR_Camera = D3D_Main.CreateFrame(FR_Root)
 Set FR_Light = D3D_Main.CreateFrame(FR_Root)
 Set Frame_Main_Car = D3D_Main.CreateFrame(FR_Root)
 Set Frame_Car = D3D_Main.CreateFrame(FR_Root)
 Set Frame_World = D3D_Main.CreateFrame(FR_Root)

                                        
 FR_Camera.SetPosition Nothing, 1, 4, -35
 Set D3D_ViewPort = D3D_Main.CreateViewport(D3D_Device, FR_Camera, 0, 0, 640, 480)
                                                                                   
 D3D_ViewPort.SetBack 200000
  
 FR_Light.SetPosition Nothing, 1, 6, -20
 Set LT_Spot = D3D_Main.CreateLightRGB(D3DRMLIGHT_POINT, 1, 1, 1)
 FR_Light.AddLight LT_Spot
  
 Set LT_Ambient = D3D_Main.CreateLightRGB(D3DRMLIGHT_AMBIENT, 0.5, 0.5, 0.5)
 FR_Root.AddLight LT_Ambient
 

 Set BackGround = D3D_Main.LoadTexture(App.Path & "\Canyon.bmp")
 FR_Root.SetSceneBackgroundImage BackGround

 Set Object_Main_Car = D3D_Main.CreateMeshBuilder()
  Object_Main_Car.LoadFromFile App.Path & "\mafia.x", 0, 0, Nothing, Nothing
  Object_Main_Car.ScaleMesh 5, 5, 5
  Frame_Main_Car.SetPosition Frame_Main_Car, 300, 0, 300
  Frame_Main_Car.SetOrientation Frame_Main_Car, Sin5, 0, Cos5, 0, 3, 0
  Frame_Main_Car.AddVisual Object_Main_Car

 Set Object_Car = D3D_Main.CreateMeshBuilder()
  Object_Car.LoadFromFile App.Path & "\jeep.x", 0, 0, Nothing, Nothing
  Object_Car.ScaleMesh 0.17, 0.17, 0.17
  Frame_Car.SetPosition Frame_Car, 250, -6, 250
  Frame_Car.SetOrientation Frame_Car, Sin5, 0, Cos5, 0, 3, 0
  Frame_Car.AddVisual Object_Car

 Set Object_World = D3D_Main.CreateMeshBuilder()
  Object_World.LoadFromFile App.Path & "\Road.x", 0, 0, Nothing, Nothing
  Object_World.ScaleMesh 1, 1, 1
  Frame_World.SetPosition Frame_World, 0, -5, 300
  Frame_World.SetOrientation Frame_World, Sin5, 0, Cos5, 0, 5, 0
  Frame_World.SetOrientation Frame_World, Sin5, 0, Cos5, 0, 3, 0
  Frame_World.AddVisual Object_World
End Sub

Private Sub DX_Render()
Dim speed As Double
speed = 0
Const Sin6 = 8.715574E-02!
    Const Cos6 = 0.9961947!
Do While esc = False
   On Local Error Resume Next
   DoEvents
   DX_Input
   D3D_ViewPort.Clear D3DRMCLEAR_TARGET Or D3DRMCLEAR_ZBUFFER
   D3D_Device.Update
   D3D_ViewPort.Render FR_Root
    If col = True Then
   DS_Back.DrawText 0, 40, "True", False
Else
   DS_Back.DrawText 0, 40, "false", False
End If
   DS_Front.Flip Nothing, DDFLIP_WAIT
If view = 0 Then
  FR_Camera.SetPosition Frame_Main_Car, 0, 10, 50
  FR_Camera.LookAt Frame_Main_Car, Nothing, D3DRMCONSTRAIN_Z
ElseIf view = 1 Then
  FR_Camera.SetPosition Frame_Main_Car, 0, 10, -50
  FR_Camera.LookAt Frame_Main_Car, Nothing, D3DRMCONSTRAIN_Z
ElseIf view = 2 Then
  FR_Camera.SetPosition Frame_Main_Car, 0, 70, 0
  FR_Camera.LookAt Frame_Main_Car, Nothing, D3DRMCONSTRAIN_Z
ElseIf view = 3 Then
  FR_Camera.SetPosition Frame_Main_Car, 30, 20, -20
  FR_Camera.LookAt Frame_Main_Car, Nothing, D3DRMCONSTRAIN_Z
ElseIf view = 4 Then
  FR_Camera.SetPosition Frame_Main_Car, 30, 20, 20
  FR_Camera.LookAt Frame_Main_Car, Nothing, D3DRMCONSTRAIN_Z
End If
If col = False Then
If up_p = True And down_p = False And speed > -7 Then
    speed = speed - 0.5
End If
If down_p = True And up_p = False And speed < 3 Then
    speed = speed + 0.5
End If
If up_p = False And down_p = False And speed <= 4 And speed > 0 Then
    speed = speed - 0.08
End If
If down_p = False And up_p = False And speed >= -8 And speed < 0 Then
    speed = speed + 0.08
End If
If down_p = False And up_p = False And speed > -0.2 And speed < 0.2 Then
    speed = 0
End If
If DI_State.Key(DIK_A) <> 0 Then
    Frame_Main_Car.SetOrientation Frame_Main_Car, speed / 120, 0, Cos6, 0, 2, 0
End If
If DI_State.Key(DIK_D) <> 0 Then
    Frame_Main_Car.SetOrientation Frame_Main_Car, -speed / 120, 0, Cos6, 0, 2, 0
End If
   DS_BGMusic.Play DSBPLAY_LOOPING
 Frame_Main_Car.SetPosition Frame_Main_Car, 0, 0, speed
 End If
If up_k = True Then
   Frame_Car.SetPosition Frame_Car, 0, 0, -1
End If
If down_k = True Then
   Frame_Car.SetPosition Frame_Car, 0, 0, 1
End If
Check_Collision
Loop
End Sub
Private Sub DX_Input()
         Const Sin5 = 8.715574E-02!
    Const Cos5 = 0.9961947!
 DI_Device.GetDeviceStateKeyboard DI_State
  
 If DI_State.Key(DIK_ESCAPE) <> 0 Then Call DX_Exit
 
 If DI_State.Key(DIK_LEFT) <> 0 Then
   Frame_Car.SetOrientation Frame_Car, -Sin5, 0, Cos5, 0, 1, 0
 End If
 
 If DI_State.Key(DIK_RIGHT) <> 0 Then
   Frame_Car.SetOrientation Frame_Car, Sin5, 0, Cos5, 0, 1, 0
 End If
 
 If DI_State.Key(DIK_1) <> 0 Then
   FR_Camera.SetOrientation FR_Camera, -Sin5, 0, Cos5, 0, 1, 0
 End If
 
 If DI_State.Key(DIK_2) <> 0 Then
   FR_Camera.SetOrientation FR_Camera, Sin5, 0, Cos5, 0, 1, 0
 End If
End Sub
Private Sub DX_Exit()
 Call DD_Main.RestoreDisplayMode
 Call DD_Main.SetCooperativeLevel(Me.hWnd, DDSCL_NORMAL)
  Call DI_Device.Unacquire
 End
End Sub
Private Sub Form_KeyDown(KeyCode As Integer, Shift As Integer)
Select Case KeyCode
Case vbKeyUp
up_k = True
Case vbKeyDown
down_k = True
Case vbKeyW
up_p = True
Case vbKeyS
down_p = True
Case vbKeyP
key_p = True
Case vbKeyV
Check_View
End Select
End Sub
Private Sub Form_KeyUp(KeyCode As Integer, Shift As Integer)
Select Case KeyCode
Case vbKeyUp
up_k = False
Case vbKeyDown
down_k = False
Case vbKeyW
up_p = False
Case vbKeyS
down_p = False
Case vbKeyP
key_p = False
End Select
End Sub
Private Sub Form_Load()
 view = 0
 nexte = 0
 DoEvents
 DX_Init
 DX_MakeObjects
 DX_Render
 Hide
End Sub
Private Sub Check_View()
If view < 4 Then
view = view + 1
Else
view = 0
End If
End Sub
Private Sub Check_Collision()
For Cycle = 0 To 24
Select Case nexte2
Case 0
New_Vertex2 = 3528
Case 1
New_Vertex2 = 3546
Case 2
New_Vertex2 = 520
Case 3
New_Vertex2 = 606
Case 4
New_Vertex2 = 600
Case 5
New_Vertex2 = 354
Case 6
New_Vertex2 = 458
Case 7
New_Vertex2 = 456
Case 8
New_Vertex2 = 437
Case 9
New_Vertex2 = 436
Case 10
New_Vertex2 = 447
Case 11
New_Vertex2 = 449
Case 12
New_Vertex2 = 198
Case 13
New_Vertex2 = 573
Case 14
New_Vertex2 = 575
Case 15
New_Vertex2 = 545
Case 16
New_Vertex2 = 3451
Case 17
New_Vertex2 = 3482
Case 18
New_Vertex2 = 3495
Case 19
New_Vertex2 = 3455
Case 20
New_Vertex2 = 78
Case 21
New_Vertex2 = 3549
Case 22
New_Vertex2 = 372
Case 23
New_Vertex2 = 379
Case 24
New_Vertex2 = 3519
 nexte2 = 0
End Select
nexte2 = nexte2 + 1
For Cycle2 = 0 To 24
Select Case nexte
Case 0
New_Vertex = 3528
Case 1
New_Vertex = 3546
Case 2
New_Vertex = 520
Case 3
New_Vertex = 606
Case 4
New_Vertex = 600
Case 5
New_Vertex = 354
Case 6
New_Vertex = 458
Case 7
New_Vertex = 456
Case 8
New_Vertex = 437
Case 9
New_Vertex = 436
Case 10
New_Vertex = 447
Case 11
New_Vertex = 449
Case 12
New_Vertex = 198
Case 13
New_Vertex = 573
Case 14
New_Vertex = 575
Case 15
New_Vertex = 545
Case 16
New_Vertex = 3451
Case 17
New_Vertex = 3482
Case 18
New_Vertex = 3495
Case 19
New_Vertex = 3455
Case 20
New_Vertex = 78
Case 21
New_Vertex = 3549
Case 22
New_Vertex = 372
Case 23
New_Vertex = 379
Case 24
New_Vertex = 3519
nexte = 0
End Select
    Frame_Main_Car.GetPosition Nothing, pos
    Object_Main_Car.GetVertex New_Vertex2, pos3
    Object_Car.GetVertex New_Vertex, pos4
    Frame_Car.GetPosition Nothing, pos2
nexte = nexte + 1
    pos.x = pos.x + pos3.x
    pos.y = pos.y + pos3.y
    pos.z = pos.z + pos3.z
    pos2.x = pos2.x + pos4.x
    pos2.y = pos2.y + pos4.y
    pos2.z = pos2.z + pos4.z
If Abs(pos.x - pos2.x) < 13 And Abs(pos.z - pos2.z) < 13 Then
    col = True
ElseIf Abs(pos.x - pos2.x) > 13 And Abs(pos.z - pos2.z) > 13 Then
    col = False
End If
If key_p = True Then
       col = False
End If
    Next Cycle2
Next Cycle

End Sub
