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

start(_ClientID, _ClientPort, _WaitUntilConnected, _DoNotReconnectOnceDisconnected,
    _TimeoutInMs, _CommThreadCycleInMs) -> stub.
finish(_ClientID) -> stub.
addStatusbarMessage(_ClientID, _Message, _OperationMode) -> stub.
appendStringSignal(_ClientID, _SignalName, _SignalValue, _SignalLength,
    _OperationMode) -> stub.
auxiliaryConsoleClose(_ClientID, _ConsoleHandle, _OperationMode) -> stub.
auxiliaryConsoleOpen(_ClientID, _Title, _MaxLines, _Mode, _PositionT, _SizeT,
    _TextColor3T, _BackgroundColor3T, _OperationMode) -> stub.
auxiliaryConsolePrint(_ClientID, _ConsoleHandle, _Text, _OperationMode) -> stub.
auxiliaryConsoleShow(_ClientID, _ConsoleHandle, _ShowState, _OperationMode) -> stub.
breakForceSensor(_ClientID, _ForceSensorHandle, _OperationMode) -> stub.
clearFloatSignal(_ClientID, _SignalName, _OperationMode) -> stub.
clearIntegerSignal(_ClientID, _SignalName, _OperationMode) -> stub.
clearStringSignal(_ClientID, _SignalName, _OperationMode) -> stub.
closeScene(_ClientID, _OperationMode) -> stub.
copyPasteObjects(_ClientID, _ObjectHandlesL, _OperationMode) -> stub.
createDummy(_ClientID, _Size, _Colors12T, _OperationMode) -> stub.
displayDialog(_ClientID, _TitleText, _MainText, _DialogType, _InitialText,
    _TitleColors6T, _DialogColors6T, _OperationMode) -> stub.
endDialog(_ClientID, _DialogHandle, _OperationMode) -> stub.
eraseFile(_ClientID, _FileNameServerSide, _OperationMode) -> stub.
getAndClearStringSignal(_ClientID, _SignalName, _OperationMode) -> stub.
getArrayParameter(_ClientID, _ParamIdentifier, _OperationMode) -> stub.
getBooleanParameter(_ClientID, _ParamIdentifier, _OperationMode) -> stub.
getCollisionHandle(_ClientID, _CollisionObjectName, _OperationMode) -> stub.
getConnectionId(_OperationMode) -> stub.
getDialogInput(_ClientID, _DialogHandle, _OperationMode) -> stub.
getDialogResult(_ClientID, _DialogHandle, _OperationMode) -> stub.
getDistanceHandle(_ClientID, _DistanceObjectName, _OperationMode) -> stub.
getFloatingParameter(_ClientID, _ParamIdentifier, _OperationMode) -> stub.
getFloatSignal(_ClientID, _SignalName, _OperationMode) -> stub.
getInMessageInfo(_ClientID, _OperationMode) -> stub.
getIntegerParameter(_ClientID, _ParamIdentifier, _OperationMode) -> stub.
getIntegerSignal(_ClientID, _SignalName, _OperationMode) -> stub.
getJointMatrix(_ClientID, _JointHandle, _OperationMode) -> stub.
getJointPosition(_ClientID, _JointHandle, _OperationMode) -> stub.
getLastCmdTime(_ClientID) -> stub.
getLastErrors(_ClientID, _OperationMode) -> stub.
getModelProperty(_ClientID,_ObjectHandle,_OperationMode) -> stub.
getObjectChild(_ClientID,_ParentObjectHandle,_ChildIndex,_OperationMode) -> stub.
getObjectFloatParameter(_ClientID,_ObjectHandle,_ParameterID,_OperationMode) -> stub.
getObjectGroupData(_ClientID,_ObjectType,_DataType,_OperationMode) -> stub.
getObjectHandle(_ClientID,_ObjectName,_OperationMode) -> stub.
getObjectIntParameter(_ClientID,_ObjectHandle,_ParameterID,_OperationMode) -> stub.
getObjectOrientation(_ClientID,_ObjectHandle,_RelativeToObjectHandle,
    _OperationMode) -> stub.
getObjectParent(_ClientID,_ObjectHandle,_OperationMode) -> stub.
getObjectPosition(_ClientID,_ObjectHandle,_RelativeToObjectHandle,_OperationMode) -> stub.
getObjects(_ClientID,_ObjectType,_OperationMode) -> stub.
getObjectSelection(_ClientID,_OperationMode) -> stub.
getObjectVelocity(_ClientID,_ObjectHandle,_OperationMode) -> stub.
getOutMessageInfo(_ClientID,_InfoType) -> stub.
getPingTime(_OperationMode) -> stub.
getStringParameter(_ClientID,_ParamIdentifier,_OperationMode) -> stub.
getStringSignal(_ClientID,_SignalName,_OperationMode) -> stub.
getUIButtonProperty(_ClientID,_UIHandle,_UIButtonID,_OperationMode) -> stub.
getUIEventButton(_ClientID,_UIHandle,_UIEventButtonID,_OperationMode) -> stub.
getUIHandle(_ClientID,_UIName,_OperationMode) -> stub.
getUISlider(_ClientID,_UIHandle,_UIButtonID,_OperationMode) -> stub.
getVisionSensorDepthBuffer(_ClientID,_SensorHandle,_OperationMode) -> stub.
getVisionSensorImage(_ClientID,_SensorHandle,_Options,_OperationMode) -> stub.
jointGetForce(_ClientID,_JointHandle,_OperationMode) -> stub.
loadModel(_ClientID,_ModelPathAndName,_Options,_OperationMode) -> stub.
loadScene(_ClientID,_ScenePathAndName,_Options,_OperationMode) -> stub.
loadUI(_ClientID,_UIPathAndName,_Options,_OperationMode) -> stub.
pauseCommunication(_ClientID,_Pause) -> stub.
pauseSimulation(_ClientID,_OperationMode) -> stub.
query(_ClientID,_SignalName,_SignalValue,_SignalLength,_RetSignalName,_TimeoutInMs) -> stub.
readCollision(_ClientID,_CollisionObjectHandle,_OperationMode) -> stub.
readDistance(_ClientID,_DistanceObjectHandle,_OperationMode) -> stub.
readForceSensor(_ClientID,_ForceSensorHandle,_OperationMode) -> stub.
readProximitySensor(_ClientID,_SensorHandle,_OperationMode) -> stub.
readVisionSensor(_ClientID,_SensorHandle,_OperationMode) -> stub.
removeObject(_ClientID,_ObjectHandle,_OperationMode) -> stub.
removeUI(_ClientID,_UIHandle,_OperationMode) -> stub.
setArrayParameter(_ClientID,_ParamIdentifier,_ParamValues3T,_OperationMode) -> stub.
setBooleanParameter(_ClientID,_ParamIdentifier,_ParamValue,_OperationMode) -> stub.
setFloatingParameter(_ClientID,_ParamIdentifier,_ParamValue,_OperationMode) -> stub.
setFloatSignal(_ClientID,_SignalName,_SignalValue,_OperationMode) -> stub.
setIntegerParameter(_ClientID,_ParamIdentifier,_ParamValue,_OperationMode) -> stub.
setIntegerSignal(_ClientID,_SignalName,_SignalValue,_OperationMode) -> stub.
setJointForce(_ClientID,_JointHandle,_Force,_OperationMode) -> stub.
setJointPosition(_ClientID,_JointHandle,_Position,_OperationMode) -> stub.
setJointTargetPosition(_ClientID,_JointHandle,_TargetPosition,_OperationMode) -> stub.
setJointTargetVelocity(_ClientID,_JointHandle,_TargetVelocity,_OperationMode) -> stub.
setModelProperty(_ClientID,_ObjectHandle,_ModelPropertiesL,_OperationMode) -> stub.
setObjectFloatParameter(_ClientID,_ObjectHandle,_ParameterID,_ParameterValue,
    _OperationMode) -> stub.
setObjectIntParameter(_ClientID,_ObjectHandle,_ParameterID,_ParameterValue,
    _OperationMode) -> stub.
setObjectOrientation(_ClientID,_ObjectHandle,_RelativeToObjectHandle,
    _EulerAngles3T,_OperationMode) -> stub.
setObjectParent(_ClientID,_ObjectHandle,_ParentObject,_KeepInPlace,_OperationMode) -> stub.
setObjectPosition(_ClientID,_ObjectHandle,_RelativeToObjectHandle,_Position3T,
    _OperationMode) -> stub.
setObjectSelection(_ClientID,_ObjectHandlesL,_OperationMode) -> stub.
setSphericalJointMatrix(_ClientID,_JointHandle,_Matrix12T,_OperationMode) -> stub.
setStringSignal(_ClientID,_SignalName,_SignalValue,_SignalLength,_OperationMode) -> stub.
setUIButtonLabel(_ClientID,_UIHandle,_UIButtonID,_UpStateLabel,_DownStateLabel,
    _OperationMode) -> stub.
setUIButtonProperty(_ClientID,_UIHandle,_UIButtonID,_ButtonPropertiesL,
    _OperationMode) -> stub.
setUISlider(_ClientID,_UIHandle,_UIButtonID,_Position,_OperationMode) -> stub.
setVisionSensorImage(_ClientID,_SensorHandle,_ImageBin,_Options,_OperationMode) -> stub.
startSimulation(_ClientID,_OperationMode) -> stub.
stopSimulation(_ClientID,_OperationMode) -> stub.
synchronous(_ClientID,_Enable) -> stub.
synchronousTrigger(_ClientID) -> stub.
transferFile(_ClientID,_FilePathAndName,_FileNameServerSide,_Timeout,
    _OperationMode) -> stub.
