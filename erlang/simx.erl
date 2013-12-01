-module(simx).

-export([
    start/6,
    finish/1,
    addStatusbarMessage/3,
    appendStringSignal/5,
    auxiliaryConsoleClose/3,
    auxiliaryConsoleOpen/9,
    auxiliaryConsolePrint/4,
    auxiliaryConsoleShow/4,
    breakForceSensor/3,
    clearFloatSignal/3,
    clearIntegerSignal/3,
    clearStringSignal/3,
    closeScene/2,
    copyPasteObjects/3,
    createDummy/4,
    displayDialog/8,
    endDialog/3,
    eraseFile/3,
    getAndClearStringSignal/3,
    getArrayParameter/3,
    getBooleanParameter/3,
    getCollisionHandle/3,
    getConnectionId/1,
    getDialogInput/3,
    getDialogResult/3,
    getDistanceHandle/3,
    getFloatingParameter/3,
    getFloatSignal/3,
    getInMessageInfo/2,
    getIntegerParameter/3,
    getIntegerSignal/3,
    getJointMatrix/3,
    getJointPosition/3,
    getLastCmdTime/1,
    getLastErrors/2,
    getModelProperty/3,
    getObjectChild/4,
    getObjectFloatParameter/4,
    getObjectGroupData/4,
    getObjectHandle/3,
    getObjectIntParameter/4,
    getObjectOrientation/4,
    getObjectParent/3,
    getObjectPosition/4,
    getObjects/3,
    getObjectSelection/2,
    getObjectVelocity/3,
    getOutMessageInfo/2,
    getPingTime/1,
    getStringParameter/3,
    getStringSignal/3,
    getUIButtonProperty/4,
    getUIEventButton/4,
    getUIHandle/3,
    getUISlider/4,
    getVisionSensorDepthBuffer/3,
    getVisionSensorImage/4,
    jointGetForce/3,
    loadModel/4,
    loadScene/4,
    loadUI/4,
    pauseCommunication/2,
    pauseSimulation/2,
    query/6,
    readCollision/3,
    readDistance/3,
    readForceSensor/3,
    readProximitySensor/3,
    readVisionSensor/3,
    removeObject/3,
    removeUI/3,
    setArrayParameter/4,
    setBooleanParameter/4,
    setFloatingParameter/4,
    setFloatSignal/4,
    setIntegerParameter/4,
    setIntegerSignal/4,
    setJointForce/4,
    setJointPosition/4,
    setJointTargetPosition/4,
    setJointTargetVelocity/4,
    setModelProperty/4,
    setObjectFloatParameter/5,
    setObjectIntParameter/5,
    setObjectOrientation/5,
    setObjectParent/5,
    setObjectPosition/5,
    setObjectSelection/3,
    setSphericalJointMatrix/4,
    setStringSignal/5,
    setUIButtonLabel/6,
    setUIButtonProperty/5,
    setUISlider/5,
    setVisionSensorImage/5,
    startSimulation/2,
    stopSimulation/2,
    synchronous/2,
    synchronousTrigger/1,
    transferFile/5]).

-on_load(init/0).

init() -> erlang:load_nif("./simxerl", 0).

-spec start(string(), integer(), integer(), integer(), integer(), integer()) -> integer().
% @doc Starts a communication thread with the server (i.e. V-REP).
%      A same client may start several communication threads. This should
%      be the very first remote API function called on the client side.
%      Make sure to start an appropriate remote API server service on the
%      server side, that will wait for a connection.
start(_ClientID, _ClientPort, _WaitUntilConnected, _DoNotReconnectOnceDisconnected,
    _TimeoutInMs, _CommThreadCycleInMs) -> stub.

-spec finish(integer()) -> atom().
finish(_ClientID) -> stub.

-type opmode() :: atom() | {atom(), integer()}.
-spec addStatusbarMessage(integer(), string(), opmode()) -> atom().
addStatusbarMessage(_ClientID, _Message, _OperationMode) -> stub.

-spec appendStringSignal(integer(), string(), string(), integer(), opmode()) -> atom().
appendStringSignal(_ClientID, _SignalName, _SignalValue, _SignalLength,
    _OperationMode) -> stub.

-spec auxiliaryConsoleClose(integer(), integer(), opmode()) -> atom().
auxiliaryConsoleClose(_ClientID, _ConsoleHandle, _OperationMode) -> stub.

-spec auxiliaryConsoleOpen(integer(), string(), integer(), integer(), {integer(),
    integer()}, {integer(), integer()}, {float(), float(), float()}, {float(),
    float(), float()}, opmode()) -> {atom(), integer()}.
auxiliaryConsoleOpen(_ClientID, _Title, _MaxLines, _Mode, _PositionT, _Size,
    _TextColor, _BackgroundColor, _OperationMode) -> stub.

-spec auxiliaryConsolePrint(integer(), integer(), string(), opmode()) -> atom().
auxiliaryConsolePrint(_ClientID, _ConsoleHandle, _Text, _OperationMode) -> stub.

-spec auxiliaryConsoleShow(integer(), integer(), integer(), opmode()) -> atom().
auxiliaryConsoleShow(_ClientID, _ConsoleHandle, _ShowState, _OperationMode) -> stub.

-spec breakForceSensor(integer(), integer(), opmode()) -> atom().
breakForceSensor(_ClientID, _ForceSensorHandle, _OperationMode) -> stub.

-spec clearFloatSignal(integer(), string(), opmode()) -> atom().
clearFloatSignal(_ClientID, _SignalName, _OperationMode) -> stub.

-spec clearIntegerSignal(integer(), string(), opmode()) -> atom().
clearIntegerSignal(_ClientID, _SignalName, _OperationMode) -> stub.

-spec clearStringSignal(integer(), string(), opmode()) -> atom().
clearStringSignal(_ClientID, _SignalName, _OperationMode) -> stub.

-spec closeScene(integer(), opmode()) -> atom().
closeScene(_ClientID, _OperationMode) -> stub.

-spec copyPasteObjects(integer(), [integer()], opmode()) -> {atom(), [integer()]}.
copyPasteObjects(_ClientID, _ObjectHandles, _OperationMode) -> stub.

-spec createDummy(integer(), float(), {byte(), byte(), byte(), byte(),
    byte(), byte(), byte(), byte(), byte(), byte(), byte(),
    byte()}, opmode()) -> {atom(), integer()}.
createDummy(_ClientID, _Size, _Colors, _OperationMode) -> stub.

-spec displayDialog(integer(), string(), string(), integer(), string(), {float(),
    float(), float(), float(), float(), float()}, {float(), float(), float(),
    float(), float(), float()}, opmode()) -> {atom(), {integer(), integer()}}.
displayDialog(_ClientID, _TitleText, _MainText, _DialogType, _InitialText,
    _TitleColors6T, _DialogColors6T, _OperationMode) -> stub.

-spec endDialog(integer(), integer(), opmode()) -> atom().
endDialog(_ClientID, _DialogHandle, _OperationMode) -> stub.

-spec eraseFile(integer(), string(), opmode()) -> atom().
eraseFile(_ClientID, _FileNameServerSide, _OperationMode) -> stub.

-spec getAndClearStringSignal(integer(), string(), opmode()) -> {atom(), string()}.
getAndClearStringSignal(_ClientID, _SignalName, _OperationMode) -> stub.

-spec getArrayParameter(integer(), integer(), opmode()) -> {atom(), {float(),
    float(), float()}}.
getArrayParameter(_ClientID, _ParamIdentifier, _OperationMode) -> stub.

-type boolean_param() :: 'hierarchy_visible' | 'console_visible' |
    'collision_handling_enabled' | 'distance_handling_enabled' |
    'ik_handling_enabled' | 'gcs_handling_enabled' |
    'dynamics_handling_enabled' | 'joint_motion_handling_enabled' |
    'path_motion_handling_enabled' | 'proximity_sensor_handling_enabled' |
    'vision_sensor_handling_enabled' | 'mill_handling_enabled' |
    'browser_visible' | 'scene_and_model_load_messages' | 'sim_reserved0' |
    'shape_textures_are_visible' | 'display_enabled' | 'infotext_visible' |
    'statustext_open' | 'fog_enabled' | 'rml2_available' | 'rml4_available' |
    'mirrors_enabled' | 'aux_clip_planes_enabled' |
    'full_model_copy_from_api' | 'realtime_simulation' | 'use_glfinish_cmd' |
    'force_show_wireless_emission' | 'force_show_wireless_reception' |
    'video_recording_triggered' | 'opengl_frame_callback_enabled' |
    'opengl_camera_view_callback_enabled' | 'threaded_rendering_enabled' |
    'fullscreen'.
-spec getBooleanParameter(integer(), boolean_param(), opmode()) -> {atom(), string()}.
getBooleanParameter(_ClientID, _ParamIdentifier, _OperationMode) -> stub.

-spec getCollisionHandle(integer(), string(), opmode()) -> {atom(), integer()}.
getCollisionHandle(_ClientID, _CollisionObjectName, _OperationMode) -> stub.

-spec getConnectionId(integer()) -> integer().
getConnectionId(_ClientID) -> stub.

-spec getDialogInput(integer(), integer(), opmode()) -> {atom(), string()}.
getDialogInput(_ClientID, _DialogHandle, _OperationMode) -> stub.

-spec getDialogResult(integer(), integer(), opmode()) -> {atom(), integer()}.
getDialogResult(_ClientID, _DialogHandle, _OperationMode) -> stub.

-spec getDistanceHandle(integer(), string(), opmode()) -> {atom(), integer()}.
getDistanceHandle(_ClientID, _DistanceObjectName, _OperationMode) -> stub.

-type float_param() :: 'rand' | 'simulation_time_step'.
-spec getFloatingParameter(integer(), float_param(), opmode()) -> {atom(), float()}.
getFloatingParameter(_ClientID, _ParamIdentifier, _OperationMode) -> stub.

-spec getFloatSignal(integer(), string(), opmode()) -> {atom(), float()}.
getFloatSignal(_ClientID, _SignalName, _OperationMode) -> stub.

-type in_message() :: 'version' | 'message_id' | 'client_time' | 'server_time' |
    'scene_id' | 'server_state'.
-spec getInMessageInfo(integer(), in_message()) -> {atom(), integer()}.
getInMessageInfo(_ClientID, _InboxMessageInfoType) -> stub.

-type integer_param() :: 'error_report_mode' | 'program_version' |
    'instance_count' | 'custom_cmd_start_id' | 'compilation_version' |
    'current_page' | 'flymode_camera_handle' | 'dynamic_step_divider' |
    'dynamic_engine' | 'server_port_start' | 'server_port_range' |
    'visible_layers' | 'infotext_style' | 'settings' | 'edit_mode_type' |
    'server_port_next' | 'qt_version' | 'event_flags_read' |
    'event_flags_read_clear' | 'platform' | 'scene_unique_id' |
    'work_thread_count' | 'mouse_x' | 'mouse_y' | 'core_count' |
    'work_thread_calc_time_ms' | 'idle_fps' | 'prox_sensor_select_down' |
    'prox_sensor_select_up'.
-spec getIntegerParameter(integer(), integer_param(), opmode()) -> {atom(), integer()}.
getIntegerParameter(_ClientID, _ParamIdentifier, _OperationMode) -> stub.

-spec getIntegerSignal(integer(), string(), opmode()) -> {atom(), integer()}.
getIntegerSignal(_ClientID, _SignalName, _OperationMode) -> stub.

-spec getJointMatrix(integer(), integer(), opmode()) -> {atom(), [float()]}.
getJointMatrix(_ClientID, _JointHandle, _OperationMode) -> stub.

-spec getJointPosition(integer(), integer(), opmode()) -> {atom(), float()}.
getJointPosition(_ClientID, _JointHandle, _OperationMode) -> stub.

-spec getLastCmdTime(integer()) -> integer().
getLastCmdTime(_ClientID) -> stub.

-spec getLastErrors(integer(), opmode()) -> {atom(), [string()]}.
getLastErrors(_ClientID, _OperationMode) -> stub.

-type model_prop() :: 'not_collidable' | 'not_measurable' | 'not_renderable' |
    'not_detectable' | 'not_cuttable' | 'not_dynamic' | 'not_respondable' |
    'not_reset' | 'not_visible' | 'not_model'.
-spec getModelProperty(integer(), integer(), opmode()) -> {atom(), [model_prop()]}.
getModelProperty(_ClientID,_ObjectHandle,_OperationMode) -> stub.

-spec getObjectChild(integer(), integer(), integer(), opmode()) -> {atom(), integer()}.
getObjectChild(_ClientID,_ParentObjectHandle,_ChildIndex,_OperationMode) -> stub.

-spec getObjectFloatParameter(integer(), integer(), integer(), opmode()) ->
    {atom(), float()}.
getObjectFloatParameter(_ClientID,_ObjectHandle,_ParameterID,_OperationMode) -> stub.

-type object_type() :: 'shape_type' | 'joint_type' | 'graph_type' | 'camera_type' |
    'dummy_type' | 'proximitysensor_type' | 'path_type' | 'visionsensor_type' |
    'mill_type' | 'forcesensor_type' | 'light_type' | 'mirror_type' | 'all'.
-type data_type() :: 0..19.
-spec getObjectGroupData(integer(), object_type(), data_type(), opmode()) -> any().
getObjectGroupData(_ClientID,_ObjectType,_DataType,_OperationMode) -> stub.

-spec getObjectHandle(integer(), string(), opmode()) -> {atom(), integer()}.
getObjectHandle(_ClientID,_ObjectName,_OperationMode) -> stub.

-spec getObjectIntParameter(integer(), integer(), integer(), opmode()) -> {atom(), integer()}.
getObjectIntParameter(_ClientID,_ObjectHandle,_ParameterID,_OperationMode) -> stub.

-spec getObjectOrientation(integer(), integer(), integer(), opmode()) -> {atom(),
    {float(), float(), float()}}.
getObjectOrientation(_ClientID,_ObjectHandle,_RelativeToObjectHandle,
    _OperationMode) -> stub.

-spec getObjectParent(integer(), integer(), opmode()) -> {atom(), integer()}.
getObjectParent(_ClientID,_ObjectHandle,_OperationMode) -> stub.

-spec getObjectPosition(integer(), integer(), integer(), opmode()) -> {atom(),
    {float(), float(), float()}}.
getObjectPosition(_ClientID,_ObjectHandle,_RelativeToObjectHandle,_OperationMode) -> stub.

-spec getObjects(integer(), object_type(), opmode()) -> {atom(), [integer()]}.
getObjects(_ClientID,_ObjectType,_OperationMode) -> stub.

-spec getObjectSelection(integer(), opmode()) -> {atom(), [integer()]}.
getObjectSelection(_ClientID,_OperationMode) -> stub.

-spec getObjectVelocity(integer(), integer(), opmode()) -> {atom(), {float(),
    float(), float()}, {float(), float(), float()}}.
getObjectVelocity(_ClientID,_ObjectHandle,_OperationMode) -> stub.

-type out_message() :: 'version' | 'message_id' | 'client_time'.
-spec getOutMessageInfo(integer(), out_message()) -> {atom(), integer()}.
getOutMessageInfo(_ClientID,_InfoType) -> stub.

-spec getPingTime(integer()) -> {atom(), integer()}.
getPingTime(_ClientID) -> stub.

-spec getStringParameter(integer(), integer(), opmode()) -> {atom(), string()}.
getStringParameter(_ClientID,_ParamIdentifier,_OperationMode) -> stub.

-spec getStringSignal(integer(), string(), opmode()) -> {atom(), string()}.
getStringSignal(_ClientID,_SignalName,_OperationMode) -> stub.

-type button_prop() :: 'button' | 'label' | 'slider' | 'editbox' | 'staydown' |
    'enabled' | 'borderless' | 'horizontallycentered' | 'ignoremouse' | 'isdown' |
    'transparent' | 'nobackgroundcolor' | 'rollupaction' | 'closeaction' |
    'verticallycentered' | 'downupevent'.
-spec getUIButtonProperty(integer(), integer(), integer(), opmode()) -> {atom(), [button_prop()]}.
getUIButtonProperty(_ClientID,_UIHandle,_UIButtonID,_OperationMode) -> stub.

-spec getUIEventButton(integer(), integer(), integer(), opmode()) -> {atom(),
    integer(), {integer(), integer()}}.
getUIEventButton(_ClientID,_UIHandle,_UIEventButtonID,_OperationMode) -> stub.

-spec getUIHandle(integer(), string(), opmode()) -> {atom(), integer()}.
getUIHandle(_ClientID,_UIName,_OperationMode) -> stub.

-spec getUISlider(integer(), integer(), integer(), opmode()) -> {atom(), integer()}.
getUISlider(_ClientID,_UIHandle,_UIButtonID,_OperationMode) -> stub.

-spec getVisionSensorDepthBuffer(integer(), integer(), opmode()) -> {atom(),
    {integer(), integer()}, binary()}.
getVisionSensorDepthBuffer(_ClientID,_SensorHandle,_OperationMode) -> stub.

-type color_mode() :: 'grayscale' | 'rgb'.
-spec getVisionSensorImage(integer(), integer(), integer(), opmode()) ->
    {atom(), {color_mode(), integer(), integer()}, binary()}.
getVisionSensorImage(_ClientID,_SensorHandle,_Options,_OperationMode) -> stub.

-spec jointGetForce(integer(), integer(), opmode()) -> {atom(), float()}.
jointGetForce(_ClientID,_JointHandle,_OperationMode) -> stub.

-spec loadModel(integer(), string(), integer(), opmode()) -> {atom(), integer()}.
loadModel(_ClientID,_ModelPathAndName,_Options,_OperationMode) -> stub.

-spec loadScene(integer(), string(), integer(), opmode()) -> atom().
loadScene(_ClientID,_ScenePathAndName,_Options,_OperationMode) -> stub.

-spec loadUI(integer(), string(), integer(), opmode()) -> {atom(), [integer()]}.
loadUI(_ClientID,_UIPathAndName,_Options,_OperationMode) -> stub.

-spec pauseCommunication(integer(), integer()) -> integer().
pauseCommunication(_ClientID,_Pause) -> stub.

-spec pauseSimulation(integer(), opmode()) -> atom().
pauseSimulation(_ClientID,_OperationMode) -> stub.

-spec query(integer(), string(), string(), integer(), string(), integer()) ->
    {atom(), string()}.
query(_ClientID,_SignalName,_SignalValue,_SignalLength,_RetSignalName,_TimeoutInMs) -> stub.

-spec readCollision(integer(), integer(), opmode()) -> {atom(), float()}.
readCollision(_ClientID,_CollisionObjectHandle,_OperationMode) -> stub.

-spec readDistance(integer(), integer(), opmode()) -> {atom(), float()}.
readDistance(_ClientID,_DistanceObjectHandle,_OperationMode) -> stub.

-spec readForceSensor(integer(), integer(), opmode()) -> {atom(), {float(),
    float(), float()}, {float(), float(), float()}}.
readForceSensor(_ClientID,_ForceSensorHandle,_OperationMode) -> stub.

-spec readProximitySensor(integer(), integer(), opmode()) -> {atom(), integer(),
    {{float(), float(), float()}, integer(), {float(), float(), float()}}}.
readProximitySensor(_ClientID,_SensorHandle,_OperationMode) -> stub.

-spec readVisionSensor(integer(), integer(), opmode()) -> {atom(), integer(),
    [[float()]]}.
readVisionSensor(_ClientID,_SensorHandle,_OperationMode) -> stub.

-spec removeObject(integer(), integer(), opmode()) -> atom().
removeObject(_ClientID,_ObjectHandle,_OperationMode) -> stub.

-spec removeUI(integer(), integer(), opmode()) -> atom().
removeUI(_ClientID,_UIHandle,_OperationMode) -> stub.

-spec setArrayParameter(integer(), integer(), {float(), float(), float()},
    opmode()) -> atom().
setArrayParameter(_ClientID,_ParamIdentifier,_ParamValues,_OperationMode) -> stub.

-spec setBooleanParameter(integer(), boolean_param(), integer(), opmode()) -> atom().
setBooleanParameter(_ClientID,_ParamIdentifier,_ParamValue,_OperationMode) -> stub.

-spec setFloatingParameter(integer(), float_param(), float(), opmode()) -> atom().
setFloatingParameter(_ClientID,_ParamIdentifier,_ParamValue,_OperationMode) -> stub.

-spec setFloatSignal(integer(), string(), float(), opmode()) -> atom().
setFloatSignal(_ClientID,_SignalName,_SignalValue,_OperationMode) -> stub.

-spec setIntegerParameter(integer(), integer_param(), integer(), opmode()) -> atom().
setIntegerParameter(_ClientID,_ParamIdentifier,_ParamValue,_OperationMode) -> stub.

-spec setIntegerSignal(integer(), string(), integer(), opmode()) -> atom().
setIntegerSignal(_ClientID,_SignalName,_SignalValue,_OperationMode) -> stub.

-spec setJointForce(integer(), integer(), float(), opmode()) -> atom().
setJointForce(_ClientID,_JointHandle,_Force,_OperationMode) -> stub.

-spec setJointPosition(integer(), integer(), float(), opmode()) -> atom().
setJointPosition(_ClientID,_JointHandle,_Position,_OperationMode) -> stub.

-spec setJointTargetPosition(integer(), integer(), float(), opmode()) -> atom().
setJointTargetPosition(_ClientID,_JointHandle,_TargetPosition,_OperationMode) -> stub.

-spec setJointTargetVelocity(integer(), integer(), float(), opmode()) -> atom().
setJointTargetVelocity(_ClientID,_JointHandle,_TargetVelocity,_OperationMode) -> stub.

-spec setModelProperty(integer(), integer(), [model_prop()], opmode()) -> atom().
setModelProperty(_ClientID,_ObjectHandle,_ModelProperties,_OperationMode) -> stub.

-spec setObjectFloatParameter(integer(), integer(), integer(), float(), opmode()) -> atom().
setObjectFloatParameter(_ClientID,_ObjectHandle,_ParameterID,_ParameterValue,
    _OperationMode) -> stub.

-spec setObjectIntParameter(integer(), integer(), integer(), integer(), opmode()) -> atom().
setObjectIntParameter(_ClientID,_ObjectHandle,_ParameterID,_ParameterValue,
    _OperationMode) -> stub.

-spec setObjectOrientation(integer(), integer(), integer(), {float(), float(),
    float()}, opmode()) -> atom().
setObjectOrientation(_ClientID,_ObjectHandle,_RelativeToObjectHandle,
    _EulerAngles,_OperationMode) -> stub.

-spec setObjectParent(integer(), integer(), integer(), integer(), opmode()) -> atom().
setObjectParent(_ClientID,_ObjectHandle,_ParentObject,_KeepInPlace,_OperationMode) -> stub.

-spec setObjectPosition(integer(), integer(), integer(), {float(), float(),
    float()}, opmode()) -> atom().
setObjectPosition(_ClientID,_ObjectHandle,_RelativeToObjectHandle,_Position,
    _OperationMode) -> stub.

-spec setObjectSelection(integer(), [integer()], opmode()) -> atom().
setObjectSelection(_ClientID,_ObjectHandles,_OperationMode) -> stub.

-spec setSphericalJointMatrix(integer(), integer(), {float(), float(), float(),
    float(), float(), float(), float(), float(), float(), float(), float(),
    float()}, opmode()) -> atom().
setSphericalJointMatrix(_ClientID,_JointHandle,_Matrix,_OperationMode) -> stub.

-spec setStringSignal(integer(), string(), string(), integer(), opmode()) -> atom().
setStringSignal(_ClientID,_SignalName,_SignalValue,_SignalLength,_OperationMode) -> stub.

-spec setUIButtonLabel(integer(), integer(), integer(), string(), string(),
    opmode()) -> atom().
setUIButtonLabel(_ClientID,_UIHandle,_UIButtonID,_UpStateLabel,_DownStateLabel,
    _OperationMode) -> stub.

-spec setUIButtonProperty(integer(), integer(), integer(), [button_prop()],
    opmode()) -> atom().
setUIButtonProperty(_ClientID,_UIHandle,_UIButtonID,_ButtonPropertiesL,
    _OperationMode) -> stub.

-spec setUISlider(integer(), integer(), integer(), integer(), opmode()) -> atom().
setUISlider(_ClientID,_UIHandle,_UIButtonID,_Position,_OperationMode) -> stub.

-spec setVisionSensorImage(integer(), integer(), binary(), color_mode(),
    opmode()) -> atom().
setVisionSensorImage(_ClientID,_SensorHandle,_ImageBin,_Options,_OperationMode) -> stub.

-spec startSimulation(integer(), opmode()) -> atom().
startSimulation(_ClientID,_OperationMode) -> stub.

-spec stopSimulation(integer(), opmode()) -> atom().
stopSimulation(_ClientID,_OperationMode) -> stub.

-spec synchronous(integer(), integer()) -> atom().
synchronous(_ClientID,_Enable) -> stub.

-spec synchronousTrigger(integer()) -> atom().
synchronousTrigger(_ClientID) -> stub.

-spec transferFile(integer(), string(), string(), integer(), opmode()) -> atom().
transferFile(_ClientID,_FilePathAndName,_FileNameServerSide,_Timeout,
    _OperationMode) -> stub.
