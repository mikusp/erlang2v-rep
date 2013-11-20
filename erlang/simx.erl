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
    createDummy/3,
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
    setVisionSensorImage/6,
    startSimulation/2,
    stopSimulation/2,
    synchronous/2,
    synchronousTrigger/1,
    transferFile/5]).

-on_load(init/0).

init() -> erlang:load_nif("./simxerl", 0).

start(_, _, _, _, _, _) -> stub.
finish(_) -> stub.
addStatusbarMessage(_, _, _) -> stub.
appendStringSignal(_, _, _, _, _) -> stub.
auxiliaryConsoleClose(_, _, _) -> stub.
auxiliaryConsoleOpen(_, _, _, _, _, _, _, _, _) -> stub.
auxiliaryConsolePrint(_, _, _, _) -> stub.
auxiliaryConsoleShow(_, _, _, _) -> stub.
breakForceSensor(_, _, _) -> stub.
clearFloatSignal(_, _, _) -> stub.
clearIntegerSignal(_, _, _) -> stub.
clearStringSignal(_, _, _) -> stub.
closeScene(_, _) -> stub.
copyPasteObjects(_, _, _) -> stub.
createDummy(_, _, _) -> stub.
displayDialog(_, _, _, _, _, _, _, _) -> stub.
endDialog(_, _, _) -> stub.
eraseFile(_, _, _) -> stub.
getAndClearStringSignal(_, _, _) -> stub.
getArrayParameter(_, _, _) -> stub.
getBooleanParameter(_, _, _) -> stub.
getCollisionHandle(_, _, _) -> stub.
getConnectionId(_) -> stub.
getDialogInput(_, _, _) -> stub.
getDialogResult(_, _, _) -> stub.
getDistanceHandle(_, _, _) -> stub.
getFloatingParameter(_, _, _) -> stub.
getFloatSignal(_, _, _) -> stub.
getInMessageInfo(_, _) -> stub.
getIntegerParameter(_, _, _) -> stub.
getIntegerSignal(_, _, _) -> stub.
getJointMatrix(_, _, _) -> stub.
getJointPosition(_, _, _) -> stub.
getLastCmdTime(_) -> stub.
getLastErrors(_, _) -> stub.
getModelProperty(_,_,_) -> stub.
getObjectChild(_,_,_,_) -> stub.
getObjectFloatParameter(_,_,_,_) -> stub.
getObjectGroupData(_,_,_,_) -> stub.
getObjectHandle(_,_,_) -> stub.
getObjectIntParameter(_,_,_,_) -> stub.
getObjectOrientation(_,_,_,_) -> stub.
getObjectParent(_,_,_) -> stub.
getObjectPosition(_,_,_,_) -> stub.
getObjects(_,_,_) -> stub.
getObjectSelection(_,_) -> stub.
getObjectVelocity(_,_,_) -> stub.
getOutMessageInfo(_,_) -> stub.
getPingTime(_) -> stub.
getStringParameter(_,_,_) -> stub.
getStringSignal(_,_,_) -> stub.
getUIButtonProperty(_,_,_,_) -> stub.
getUIEventButton(_,_,_,_) -> stub.
getUIHandle(_,_,_) -> stub.
getUISlider(_,_,_,_) -> stub.
getVisionSensorDepthBuffer(_,_,_) -> stub.
getVisionSensorImage(_,_,_,_) -> stub.
jointGetForce(_,_,_) -> stub.
loadModel(_,_,_,_) -> stub.
loadScene(_,_,_,_) -> stub.
loadUI(_,_,_,_) -> stub.
pauseCommunication(_,_) -> stub.
pauseSimulation(_,_) -> stub.
query(_,_,_,_,_,_) -> stub.
readCollision(_,_,_) -> stub.
readDistance(_,_,_) -> stub.
readForceSensor(_,_,_) -> stub.
readProximitySensor(_,_,_) -> stub.
readVisionSensor(_,_,_) -> stub.
removeObject(_,_,_) -> stub.
removeUI(_,_,_) -> stub.
setArrayParameter(_,_,_,_) -> stub.
setBooleanParameter(_,_,_,_) -> stub.
setFloatingParameter(_,_,_,_) -> stub.
setFloatSignal(_,_,_,_) -> stub.
setIntegerParameter(_,_,_,_) -> stub.
setIntegerSignal(_,_,_,_) -> stub.
setJointForce(_,_,_,_) -> stub.
setJointPosition(_,_,_,_) -> stub.
setJointTargetPosition(_,_,_,_) -> stub.
setJointTargetVelocity(_,_,_,_) -> stub.
setModelProperty(_,_,_,_) -> stub.
setObjectFloatParameter(_,_,_,_,_) -> stub.
setObjectIntParameter(_,_,_,_,_) -> stub.
setObjectOrientation(_,_,_,_,_) -> stub.
setObjectParent(_,_,_,_,_) -> stub.
setObjectPosition(_,_,_,_,_) -> stub.
setObjectSelection(_,_,_) -> stub.
setSphericalJointMatrix(_,_,_,_) -> stub.
setStringSignal(_,_,_,_,_) -> stub.
setUIButtonLabel(_,_,_,_,_,_) -> stub.
setUIButtonProperty(_,_,_,_,_) -> stub.
setUISlider(_,_,_,_,_) -> stub.
setVisionSensorImage(_,_,_,_,_,_) -> stub.
startSimulation(_,_) -> stub.
stopSimulation(_,_) -> stub.
synchronous(_,_) -> stub.
synchronousTrigger(_) -> stub.
transferFile(_,_,_,_,_) -> stub.
