#include <erl_nif.h>
#include <extApi.h>
#include <string.h>

#define ERL_FUNC(funname) static ERL_NIF_TERM funname(ErlNifEnv *env, \
    int argc, const ERL_NIF_TERM argv[])

#define PARAM(type, number, name) type name; \
    if (!enif_get_##type(env, argv[number], &name)) \
        return enif_make_badarg(env);

#define SPARAM(number, name) char name[256]; \
    if (!enif_get_string(env, argv[number], name, 256, ERL_NIF_LATIN1)) \
        return enif_make_badarg(env);

#define APARAM(number, name) char name[256]; \
    if (!enif_get_atom(env, argv[number], name, 256, ERL_NIF_LATIN1)) \
        return enif_make_badarg(env);

#define OPMODE(number, name) APARAM(number, opMode); \
    int name = operationModeAtomToInt(opMode); \
    if (name == -1) \
        return enif_make_badarg(env);

#define LIST(type, number, listName, listLength) \
    ERL_NIF_TERM l##listName, h##listName, t##listName; \
    type* listName = NULL; \
    unsigned listLength, i##listLength; \
    \
    if (!enif_get_list_length(env, argv[number], &listLength)) \
        return enif_make_badarg(env); \
    l##listName = argv[number]; \
    listName = malloc(sizeof(type) * listLength); \
    \
    for (i##listLength = 0; i##listLength < listLength; ++i##listLength) { \
        enif_get_list_cell(env, l##listName, &h##listName, &t##listName); \
        if (!enif_get_##type(env, h##listName, &listName[i##listLength])) { \
            free(listName); \
            return enif_make_badarg(env); \
        } \
    }

#define TUPLE(type, number, name, desiredArity) \
    const ERL_NIF_TERM* nif##name; \
    type name[desiredArity]; \
    int a##name, i##name; \
    \
    if (!enif_get_tuple(env, argv[number], &a##name, &nif##name)) \
        return enif_make_badarg(env); \
    if (desiredArity != a##name) \
        return enif_make_badarg(env); \
    for (; i##name < a##name; ++i##name) \
        if (!enif_get_##type(env, nif##name[i##name], &name[i##name])) \
            return enif_make_badarg(env);

#define D2F(doubleArray, floatArray, length) \
    float floatArray[length]; \
    unsigned i##floatArray = 0; \
    for (; i##floatArray < length; ++i##floatArray) \
        floatArray[i##floatArray] = (float)doubleArray[i##floatArray];


int operationModeAtomToInt(const char* atom) {
    if (strcmp(atom, "oneshot") == 0)
        return simx_opmode_oneshot;
    else if (strcmp(atom, "oneshot_wait") == 0)
        return simx_opmode_oneshot_wait;
    else if (strcmp(atom, "streaming") == 0)
        return simx_opmode_streaming;
    else if (strcmp(atom, "continuous") == 0)
        return simx_opmode_continuous;
    else if (strcmp(atom, "oneshot_split") == 0)
        return simx_opmode_oneshot_split;
    else if (strcmp(atom, "streaming_split") == 0)
        return simx_opmode_streaming_split;
    else if (strcmp(atom, "continuous_split") == 0)
        return simx_opmode_continuous_split;
    else if (strcmp(atom, "discontinue") == 0)
        return simx_opmode_discontinue;
    else if (strcmp(atom, "buffer") == 0)
        return simx_opmode_buffer;
    else if (strcmp(atom, "remove") == 0)
        return simx_opmode_remove;
    else
        return -1;
}

int boolParamAtomToInt(const char* atom) {
    if (strcmp(atom, "hierarchy_visible") == 0) return sim_boolparam_hierarchy_visible;
    else if (strcmp(atom, "console_visible") == 0) return sim_boolparam_console_visible;
    else if (strcmp(atom, "collision_handling_enabled") == 0) return sim_boolparam_collision_handling_enabled;
    else if (strcmp(atom, "distance_handling_enabled") == 0) return sim_boolparam_distance_handling_enabled;
    else if (strcmp(atom, "ik_handling_enabled") == 0) return sim_boolparam_ik_handling_enabled;
    else if (strcmp(atom, "gcs_handling_enabled") == 0) return sim_boolparam_gcs_handling_enabled;
    else if (strcmp(atom, "dynamics_handling_enabled") == 0) return sim_boolparam_dynamics_handling_enabled;
    else if (strcmp(atom, "joint_motion_handling_enabled") == 0) return sim_boolparam_joint_motion_handling_enabled;
    else if (strcmp(atom, "path_motion_handling_enabled") == 0) return sim_boolparam_path_motion_handling_enabled;
    else if (strcmp(atom, "proximity_sensor_handling_enabled") == 0) return sim_boolparam_proximity_sensor_handling_enabled;
    else if (strcmp(atom, "vision_sensor_handling_enabled") == 0) return sim_boolparam_vision_sensor_handling_enabled;
    else if (strcmp(atom, "mill_handling_enabled") == 0) return sim_boolparam_mill_handling_enabled;
    else if (strcmp(atom, "browser_visible") == 0) return sim_boolparam_browser_visible;
    else if (strcmp(atom, "scene_and_model_load_messages") == 0) return sim_boolparam_scene_and_model_load_messages;
    else if (strcmp(atom, "sim_reserved0") == 0) return sim_reserved0;
    else if (strcmp(atom, "shape_textures_are_visible") == 0) return sim_boolparam_shape_textures_are_visible;
    else if (strcmp(atom, "display_enabled") == 0) return sim_boolparam_display_enabled;
    else if (strcmp(atom, "infotext_visible") == 0) return sim_boolparam_infotext_visible;
    else if (strcmp(atom, "statustext_open") == 0) return sim_boolparam_statustext_open;
    else if (strcmp(atom, "fog_enabled") == 0) return sim_boolparam_fog_enabled;
    else if (strcmp(atom, "rml2_available") == 0) return sim_boolparam_rml2_available;
    else if (strcmp(atom, "rml4_available") == 0) return sim_boolparam_rml4_available;
    else if (strcmp(atom, "mirrors_enabled") == 0) return sim_boolparam_mirrors_enabled;
    else if (strcmp(atom, "aux_clip_planes_enabled") == 0) return sim_boolparam_aux_clip_planes_enabled;
    else if (strcmp(atom, "full_model_copy_from_api") == 0) return sim_boolparam_full_model_copy_from_api;
    else if (strcmp(atom, "realtime_simulation") == 0) return sim_boolparam_realtime_simulation;
    else if (strcmp(atom, "use_glfinish_cmd") == 0) return sim_boolparam_use_glfinish_cmd;
    else if (strcmp(atom, "force_show_wireless_emission") == 0) return sim_boolparam_force_show_wireless_emission;
    else if (strcmp(atom, "force_show_wireless_reception") == 0) return sim_boolparam_force_show_wireless_reception;
    else if (strcmp(atom, "video_recording_triggered") == 0) return sim_boolparam_video_recording_triggered;
    else if (strcmp(atom, "opengl_frame_callback_enabled") == 0) return sim_boolparam_opengl_frame_callback_enabled;
    else if (strcmp(atom, "opengl_camera_view_callback_enabled") == 0) return sim_boolparam_opengl_camera_view_callback_enabled;
    else if (strcmp(atom, "threaded_rendering_enabled") == 0) return sim_boolparam_threaded_rendering_enabled;
    else if (strcmp(atom, "fullscreen") == 0) return sim_boolparam_fullscreen;
    else
        return -1;
}

int floatParamAtomToInt(const char* atom) {
    if (strcmp(atom, "rand") == 0) return sim_floatparam_rand;
    else if (strcmp(atom, "simulation_time_step") == 0) return sim_floatparam_simulation_time_step;
    else
        return -1;
}

int integerParamAtomToInt(const char* atom) {
    if (strcmp(atom, "error_report_mode") == 0) return sim_intparam_error_report_mode;
    else if (strcmp(atom, "program_version") == 0) return sim_intparam_program_version;
    else if (strcmp(atom, "instance_count") == 0) return sim_intparam_instance_count;
    else if (strcmp(atom, "custom_cmd_start_id") == 0) return sim_intparam_custom_cmd_start_id;
    else if (strcmp(atom, "compilation_version") == 0) return sim_intparam_compilation_version;
    else if (strcmp(atom, "current_page") == 0) return sim_intparam_current_page;
    else if (strcmp(atom, "flymode_camera_handle") == 0) return sim_intparam_flymode_camera_handle;
    else if (strcmp(atom, "dynamic_step_divider") == 0) return sim_intparam_dynamic_step_divider;
    else if (strcmp(atom, "dynamic_engine") == 0) return sim_intparam_dynamic_engine;
    else if (strcmp(atom, "server_port_start") == 0) return sim_intparam_server_port_start;
    else if (strcmp(atom, "server_port_range") == 0) return sim_intparam_server_port_range;
    else if (strcmp(atom, "visible_layers") == 0) return sim_intparam_visible_layers;
    else if (strcmp(atom, "infotext_style") == 0) return sim_intparam_infotext_style;
    else if (strcmp(atom, "settings") == 0) return sim_intparam_settings;
    else if (strcmp(atom, "edit_mode_type") == 0) return sim_intparam_edit_mode_type;
    else if (strcmp(atom, "server_port_next") == 0) return sim_intparam_server_port_next;
    else if (strcmp(atom, "qt_version") == 0) return sim_intparam_qt_version;
    else if (strcmp(atom, "event_flags_read") == 0) return sim_intparam_event_flags_read;
    else if (strcmp(atom, "event_flags_read_clear") == 0) return sim_intparam_event_flags_read_clear;
    else if (strcmp(atom, "platform") == 0) return sim_intparam_platform;
    else if (strcmp(atom, "scene_unique_id") == 0) return sim_intparam_scene_unique_id;
    else if (strcmp(atom, "work_thread_count") == 0) return sim_intparam_work_thread_count;
    else if (strcmp(atom, "mouse_x") == 0) return sim_intparam_mouse_x;
    else if (strcmp(atom, "mouse_y") == 0) return sim_intparam_mouse_y;
    else if (strcmp(atom, "core_count") == 0) return sim_intparam_core_count;
    else if (strcmp(atom, "work_thread_calc_time_ms") == 0) return sim_intparam_work_thread_calc_time_ms;
    else if (strcmp(atom, "idle_fps") == 0) return sim_intparam_idle_fps;
    else if (strcmp(atom, "prox_sensor_select_down") == 0) return sim_intparam_prox_sensor_select_down;
    else if (strcmp(atom, "prox_sensor_select_up") == 0) return sim_intparam_prox_sensor_select_up;
    else
        return -1;
}

int modelPropAtomToInt(const char* atom) {
    if (strcmp(atom, "not_collidable") == 0) return sim_modelproperty_not_collidable;
    else if (strcmp(atom, "not_measurable") == 0) return sim_modelproperty_not_measurable;
    else if (strcmp(atom, "not_renderable") == 0) return sim_modelproperty_not_renderable;
    else if (strcmp(atom, "not_detectable") == 0) return sim_modelproperty_not_detectable;
    else if (strcmp(atom, "not_cuttable") == 0) return sim_modelproperty_not_cuttable;
    else if (strcmp(atom, "not_dynamic") == 0) return sim_modelproperty_not_dynamic;
    else if (strcmp(atom, "not_respondable") == 0) return sim_modelproperty_not_respondable;
    else if (strcmp(atom, "not_reset") == 0) return sim_modelproperty_not_reset;
    else if (strcmp(atom, "not_visible") == 0) return sim_modelproperty_not_visible;
    else if (strcmp(atom, "not_model") == 0) return sim_modelproperty_not_model;
    else
        return -1;
}

ERL_FUNC(start) {
    SPARAM(0, clientAddress);
    PARAM(int, 1, clientPort);
    PARAM(int, 2, waitUntilConnected);
    PARAM(int, 3, doNotReconnectOnceDisconnected);
    PARAM(int, 4, timeoutInMs);
    PARAM(int, 5, commThreadCycleInMs);

    int ret = simxStart(clientAddress, clientPort, (char)waitUntilConnected,
        (char)doNotReconnectOnceDisconnected, timeoutInMs, commThreadCycleInMs);
    return enif_make_int(env, ret);
}

ERL_FUNC(finish) {
    PARAM(int, 0, clientID);

    simxFinish(clientID);

    return enif_make_atom(env, "ok");
}

ERL_FUNC(addStatusbarMessage) {
    PARAM(int, 0, clientID);
    SPARAM(1, message);
    OPMODE(2, operationMode);

    int ret = simxAddStatusbarMessage(clientID, message, operationMode);
    return enif_make_int(env, ret);
}

ERL_FUNC(appendStringSignal) {
    PARAM(int, 0, clientID);
    SPARAM(1, signalName);
    PARAM(int, 3, signalLength);
    OPMODE(4, operationMode);

    char* signalValueToAppend = malloc(sizeof(simxChar)*signalLength);
    enif_get_string(env, argv[2], signalValueToAppend, signalLength, ERL_NIF_LATIN1);

    int ret = simxAppendStringSignal(clientID, signalName, signalValueToAppend,
        signalLength, operationMode);

    free(signalValueToAppend);

    return enif_make_int(env, ret);
}

ERL_FUNC(auxiliaryConsoleClose) {
    PARAM(int, 0, clientID);
    PARAM(int, 1, consoleHandle);
    OPMODE(2, operationMode);

    int ret = simxAuxiliaryConsoleClose(clientID, consoleHandle, operationMode);
    return enif_make_int(env, ret);
}

ERL_FUNC(auxiliaryConsoleOpen) {
    PARAM(int, 0, clientID);
    SPARAM(1, title);
    PARAM(int, 2, maxLines);
    PARAM(int, 3, mode);
    TUPLE(int, 4, position, 2);
    TUPLE(int, 5, size, 2);
    TUPLE(double, 6, textColor, 3);
    TUPLE(double, 7, backgroundColor, 3);
    OPMODE(8, operationMode);
    int consoleHandle;

    D2F(textColor, fTextColor, 3);
    D2F(backgroundColor, fBackgroundColor, 3);

    int ret = simxAuxiliaryConsoleOpen(clientID, title, maxLines, mode,
        position, size, fTextColor, fBackgroundColor, &consoleHandle, operationMode);
    return enif_make_tuple2(env, enif_make_int(env, ret), enif_make_int(env,
        consoleHandle));
}

ERL_FUNC(auxiliaryConsolePrint) {
    PARAM(int, 0, clientID);
    PARAM(int, 1, consoleHandle);
    SPARAM(2, txt);
    OPMODE(3, operationMode);

    int ret = simxAuxiliaryConsolePrint(clientID, consoleHandle, txt,
        operationMode);
    return enif_make_int(env, ret);
}

ERL_FUNC(auxiliaryConsoleShow) {
    PARAM(int, 0, clientID);
    PARAM(int, 1, consoleHandle);
    PARAM(int, 2, showState);
    OPMODE(3, operationMode);

    int ret = simxAuxiliaryConsoleShow(clientID, consoleHandle, (char)showState, operationMode);
    return enif_make_int(env, ret);
}

ERL_FUNC(breakForceSensor) {
    PARAM(int, 0, clientID);
    PARAM(int, 1, forceSensorHandle);
    OPMODE(2, operationMode);

    int ret = simxBreakForceSensor(clientID, forceSensorHandle, operationMode);
    return enif_make_int(env, ret);
}

ERL_FUNC(clearFloatSignal) {
    PARAM(int, 0, clientID);
    SPARAM(1, signalName);
    OPMODE(2, operationMode);

    int ret = simxClearFloatSignal(clientID, signalName, operationMode);
    return enif_make_int(env, ret);
}

ERL_FUNC(clearIntegerSignal) {
    PARAM(int, 0, clientID);
    SPARAM(1, signalName);
    OPMODE(2, operationMode);

    int ret = simxClearIntegerSignal(clientID, signalName, operationMode);
    return enif_make_int(env, ret);
}

ERL_FUNC(clearStringSignal) {
    PARAM(int, 0, clientID);
    SPARAM(1, signalName);
    OPMODE(2, operationMode);

    int ret = simxClearStringSignal(clientID, signalName, operationMode);
    return enif_make_int(env, ret);
}

ERL_FUNC(closeScene) {
    PARAM(int, 0, clientID);
    OPMODE(1, operationMode);

    int ret = simxCloseScene(clientID, operationMode);
    return enif_make_int(env, ret);
}

ERL_FUNC(copyPasteObjects) {
    PARAM(int, 0, clientID);
    LIST(int, 1, objectHandles, objectCount);
    OPMODE(2, operationMode);

    int newObjectCount;
    int *newObjectHandles;

    int ret = simxCopyPasteObjects(clientID, objectHandles, objectCount,
        &newObjectHandles, &newObjectCount, operationMode);

    ERL_NIF_TERM* tmpList = malloc(sizeof(ERL_NIF_TERM)*newObjectCount);

    int j;
    for (j = 0; j < newObjectCount; ++j) {
        tmpList[j] = enif_make_int(env, newObjectHandles[j]);
    }

    ERL_NIF_TERM retList = enif_make_list_from_array(env, tmpList, newObjectCount);

    free(objectHandles);
    free(tmpList);
    return enif_make_tuple2(env, enif_make_int(env, ret), retList);
}

// returns a pointer - is it useful in erlang?
//ERL_FUNC(createBuffer) {
//    int bufferSize;
//
//    enif_get_int(env, argv[0], &bufferSize);
//
//    int ret = simxCreateBuffer(bufferSize);
//    return enif_make_int(env, ret);
//}

ERL_FUNC(createDummy) {
    PARAM(int, 0, clientID);
    PARAM(double, 1, size);
    // add custom colors support
    OPMODE(2, operationMode);
    int dummyHandle;

    int ret = simxCreateDummy(clientID, (float)size, NULL, &dummyHandle, operationMode);
    return enif_make_tuple2(env, enif_make_int(env, ret), enif_make_int(env, dummyHandle));
}

ERL_FUNC(displayDialog) {
    PARAM(int, 0, clientID);
    SPARAM(1, titleText);
    SPARAM(2, mainText);
    PARAM(int, 3, dialogType);
    SPARAM(4, initialText);
    TUPLE(double, 5, titleColors, 12);
    TUPLE(double, 6, dialogColors, 12);
    OPMODE(7, operationMode);
    int dialogHandle, uiHandle;

    D2F(titleColors, fTitleColors, 12);
    D2F(dialogColors, fDialogColors, 12);

    int ret = simxDisplayDialog(clientID, titleText, mainText, dialogType,
        initialText, fTitleColors, fDialogColors, &dialogHandle, &uiHandle,
        operationMode);

    return enif_make_tuple2(env, enif_make_int(env, ret), enif_make_tuple2(env,
        enif_make_int(env, dialogHandle), enif_make_int(env, uiHandle)));
}

ERL_FUNC(endDialog) {
    PARAM(int, 0, clientID);
    PARAM(int, 1, dialogHandle);
    OPMODE(2, operationMode);

    int ret = simxEndDialog(clientID, dialogHandle, operationMode);
    return enif_make_int(env, ret);
}

ERL_FUNC(eraseFile) {
    PARAM(int, 0, clientID);
    SPARAM(1, fileName_serverSide);
    OPMODE(2, operationMode);

    int ret = simxEraseFile(clientID, fileName_serverSide, operationMode);
    return enif_make_int(env, ret);
}

ERL_FUNC(getAndClearStringSignal) {
    PARAM(int, 0, clientID);
    SPARAM(1, signalName);
    OPMODE(2, operationMode);
    int signalLength;
    char *signalValue;

    int ret = simxGetAndClearStringSignal(clientID, signalName, &signalValue,
        &signalLength, operationMode);
    return enif_make_tuple2(env, enif_make_int(env, ret), enif_make_string_len(
        env, signalValue, (size_t)signalLength, ERL_NIF_LATIN1));
}

ERL_FUNC(getArrayParameter) {
    PARAM(int, 0, clientID);
    PARAM(int, 1, paramIdentifier);
    OPMODE(2, operationMode);
    float paramValues[3];

    int ret = simxGetArrayParameter(clientID, paramIdentifier, paramValues,
        operationMode);
    return enif_make_tuple2(env, enif_make_int(env, ret), enif_make_tuple3(env,
        enif_make_double(env, paramValues[0]),
        enif_make_double(env, paramValues[1]),
        enif_make_double(env, paramValues[2])));
}

ERL_FUNC(getBooleanParameter) {
    PARAM(int, 0, clientID);
    APARAM(1, paramIdentifier);
    OPMODE(2, operationMode);
    char paramValue[256];

    int boolParamIdentifier = boolParamAtomToInt(paramIdentifier);
    if (boolParamIdentifier == -1)
        return enif_make_badarg(env);

    int ret = simxGetBooleanParameter(clientID, boolParamIdentifier, paramValue,
        operationMode);
    return enif_make_tuple2(env, enif_make_int(env, ret), enif_make_string(
        env, paramValue, ERL_NIF_LATIN1));
}

ERL_FUNC(getCollisionHandle) {
    PARAM(int, 0, clientID);
    SPARAM(1, collisionObjectName);
    OPMODE(2, operationMode);
    int handle;

    int ret = simxGetCollisionHandle(clientID, collisionObjectName, &handle,
        operationMode);
    return enif_make_tuple2(env, enif_make_int(env, ret), enif_make_int(env,
        handle));
}

ERL_FUNC(getConnectionId) {
    PARAM(int, 0, clientID);

    int ret = simxGetConnectionId(clientID);
    return enif_make_int(env, ret);
}

ERL_FUNC(getDialogInput) {
    PARAM(int, 0, clientID);
    PARAM(int, 1, dialogHandle);
    OPMODE(2, operationMode);
    char* inputText;

    int ret = simxGetDialogInput(clientID, dialogHandle, &inputText,
        operationMode);
    return enif_make_tuple2(env, enif_make_int(env, ret), enif_make_string(env,
        inputText, ERL_NIF_LATIN1));
}

ERL_FUNC(getDialogResult) {
    PARAM(int, 0, clientID);
    PARAM(int, 1, dialogHandle);
    OPMODE(2, operationMode);
    int result;

    int ret = simxGetDialogResult(clientID, dialogHandle, &result, operationMode);
    return enif_make_tuple2(env, enif_make_int(env, ret), enif_make_int(env,
        result));
}

ERL_FUNC(getDistanceHandle) {
    PARAM(int, 0, clientID);
    SPARAM(1, distanceObjectName);
    OPMODE(2, operationMode);
    int handle;

    int ret = simxGetDistanceHandle(clientID, distanceObjectName, &handle,
        operationMode);
    return enif_make_tuple2(env, enif_make_int(env, ret), enif_make_int(env,
        handle));
}

ERL_FUNC(getFloatingParameter) {
    PARAM(int, 0, clientID);
    APARAM(1, paramIdentifier);
    OPMODE(2, operationMode);
    float paramValue;

    int floatParamIdentifier = floatParamAtomToInt(paramIdentifier);
    if (floatParamIdentifier == -1)
        return enif_make_badarg(env);

    int ret = simxGetFloatingParameter(clientID, floatParamIdentifier, &paramValue,
        operationMode);
    return enif_make_tuple2(env, enif_make_int(env, ret), enif_make_double(env,
        (double)paramValue));
}

ERL_FUNC(getFloatSignal) {
    PARAM(int, 0, clientID);
    SPARAM(1, signalName);
    OPMODE(2, operationMode);
    float signalValue;

    int ret = simxGetFloatSignal(clientID, signalName, &signalValue,
        operationMode);
    return enif_make_tuple2(env, enif_make_int(env, ret), enif_make_double(env,
        (double)signalValue));
}

ERL_FUNC(getInMessageInfo) {
    PARAM(int, 0, clientID);
    PARAM(int, 1, infoType);
    int info;

    int ret = simxGetInMessageInfo(clientID, infoType, &info);
    return enif_make_tuple2(env, enif_make_int(env, ret), enif_make_int(env,
        info));
}

ERL_FUNC(getIntegerParameter) {
    PARAM(int, 0, clientID);
    APARAM(1, paramIdentifier);
    OPMODE(2, operationMode);
    int paramValue;

    int intParamIdentifier = integerParamAtomToInt(paramIdentifier);
    if (intParamIdentifier == -1)
        return enif_make_badarg(env);

    int ret = simxGetIntegerParameter(clientID, intParamIdentifier, &paramValue,
        operationMode);
    return enif_make_tuple2(env, enif_make_int(env, ret), enif_make_int(env,
        paramValue));
}

ERL_FUNC(getIntegerSignal) {
    PARAM(int, 0, clientID);
    SPARAM(1, signalName);
    OPMODE(2, operationMode);
    int signalValue;

    int ret = simxGetIntegerSignal(clientID, signalName, &signalValue,
        operationMode);
    return enif_make_tuple2(env, enif_make_int(env, ret), enif_make_int(env,
        signalValue));
}

ERL_FUNC(getJointMatrix) {
    PARAM(int, 0, clientID);
    PARAM(int, 1, jointHandle);
    OPMODE(2, operationMode);
    float matrix[12];

    int ret = simxGetJointMatrix(clientID, jointHandle, matrix, operationMode);

    ERL_NIF_TERM retList[12];

    int i = 0;
    for (; i < 12; ++i) {
        retList[i] = enif_make_double(env, (double)matrix[i]);
    }

    return enif_make_tuple2(env, enif_make_int(env, ret), enif_make_list_from_array(env,
        retList, 12));
}

ERL_FUNC(getJointPosition) {
    PARAM(int, 0, clientID);
    PARAM(int, 1, jointHandle);
    OPMODE(2, operationMode);
    float position;

    int ret = simxGetJointPosition(clientID, jointHandle, &position,
        operationMode);
    return enif_make_tuple2(env, enif_make_int(env, ret), enif_make_double(env,
        (double)position));
}

ERL_FUNC(getLastCmdTime) {
    PARAM(int, 0, clientID);

    int ret = simxGetLastCmdTime(clientID);
    return enif_make_int(env, ret);
}

ERL_FUNC(getLastErrors) {
    PARAM(int, 0, clientID);
    OPMODE(1, operationMode);
    int errorCnt;
    char* errorStrings;

    int ret = simxGetLastErrors(clientID, &errorCnt, &errorStrings, operationMode);
    // TODO should return a list of error strings
    // conversion from '\0' delimited strings to a list in Erlang
    return enif_make_int(env, ret);
}

ERL_FUNC(getModelProperty) {
    PARAM(int, 0, clientID);
    PARAM(int, 1, objectHandle);
    OPMODE(2, operationMode);
    int prop;

    int ret = simxGetModelProperty(clientID, objectHandle, &prop, operationMode);
    return enif_make_tuple2(env, enif_make_int(env, ret), enif_make_int(env,
        prop));
}

ERL_FUNC(getObjectChild) {
    PARAM(int, 0, clientID);
    PARAM(int, 1, parentObjectHandle);
    PARAM(int, 2, childIndex);
    OPMODE(3, operationMode);
    int childObjectHandle;

    int ret = simxGetObjectChild(clientID, parentObjectHandle, childIndex,
        &childObjectHandle, operationMode);
    return enif_make_tuple2(env, enif_make_int(env, ret), enif_make_int(env,
        childObjectHandle));
}

ERL_FUNC(getObjectFloatParameter) {
    PARAM(int, 0, clientID);
    PARAM(int, 1, objectHandle);
    PARAM(int, 2, parameterID);
    OPMODE(3, operationMode);
    float parameterValue;

    int ret = simxGetObjectFloatParameter(clientID, objectHandle, parameterID,
        &parameterValue, operationMode);
    return enif_make_tuple2(env, enif_make_int(env, ret), enif_make_double(env,
        (double)parameterValue));
}

ERL_FUNC(getObjectGroupData) {
    PARAM(int, 0, clientID);
    PARAM(int, 1, objectType);
    PARAM(int, 2, dataType);
    OPMODE(3, operationMode);
    int handlesCount, intDataCount, floatDataCount, stringDataCount;
    int *handles, *intData;
    float *floatData;
    char *stringData;

    int ret = simxGetObjectGroupData(clientID, objectType, dataType, &handlesCount,
        &handles, &intDataCount, &intData, &floatDataCount, &floatData,
        &stringDataCount, &stringData, operationMode);

    // TODO now it should convert the data returned (depending on handlesCount and
    // dataType) to list or list of tuples in Erlang
    return enif_make_int(env, ret);
}

ERL_FUNC(getObjectHandle) {
    PARAM(int, 0, clientID);
    SPARAM(1, objectName);
    OPMODE(2, operationMode);
    int handle;

    int ret = simxGetObjectHandle(clientID, objectName, &handle, operationMode);
    return enif_make_tuple2(env, enif_make_int(env, ret), enif_make_int(env,
        handle));
}

ERL_FUNC(getObjectIntParameter) {
    PARAM(int, 0, clientID);
    PARAM(int, 1, objectHandle);
    PARAM(int, 2, parameterID);
    OPMODE(3, operationMode);
    int parameterValue;

    int ret = simxGetObjectIntParameter(clientID, objectHandle, parameterID,
        &parameterValue, operationMode);
    return enif_make_tuple2(env, enif_make_int(env, ret), enif_make_int(env,
        parameterValue));
}

ERL_FUNC(getObjectOrientation) {
    PARAM(int, 0, clientID);
    PARAM(int, 1, objectHandle);
    PARAM(int, 2, relativeToObjectHandle);
    OPMODE(3, operationMode);
    float eulerAngles[3];

    int ret = simxGetObjectOrientation(clientID, objectHandle,
        relativeToObjectHandle, eulerAngles, operationMode);
    return enif_make_tuple2(env, enif_make_int(env, ret), enif_make_tuple3(env,
        enif_make_double(env, (double)eulerAngles[0]),
        enif_make_double(env, (double)eulerAngles[1]),
        enif_make_double(env, (double)eulerAngles[2])));
}

ERL_FUNC(getObjectParent) {
    PARAM(int, 0, clientID);
    PARAM(int, 1, objectHandle);
    OPMODE(2, operationMode);
    int parentObjectHandle;

    int ret = simxGetObjectParent(clientID, objectHandle, &parentObjectHandle,
        operationMode);
    return enif_make_tuple2(env, enif_make_int(env, ret), enif_make_int(env,
        parentObjectHandle));
}

ERL_FUNC(getObjectPosition) {
    PARAM(int, 0, clientID);
    PARAM(int, 1, objectHandle);
    PARAM(int, 2, relativeToObjectHandle);
    OPMODE(3, operationMode);
    float position[3];

    int ret = simxGetObjectPosition(clientID, objectHandle, relativeToObjectHandle,
        position, operationMode);
    return enif_make_tuple2(env, enif_make_int(env, ret), enif_make_tuple3(env,
        enif_make_double(env, (double)position[0]),
        enif_make_double(env, (double)position[1]),
        enif_make_double(env, (double)position[2])));
}

ERL_FUNC(getObjects) {
    PARAM(int, 0, clientID);
    PARAM(int, 1, objectType);
    OPMODE(2, operationMode);
    int objectCount;
    int *objectHandles;

    int ret = simxGetObjects(clientID, objectType, &objectCount, &objectHandles,
        operationMode);

    ERL_NIF_TERM* objectHandlesArray = malloc(sizeof(ERL_NIF_TERM)*objectCount);

    int i = 0;
    for (; i < objectCount; ++i) {
        objectHandlesArray[i] = enif_make_int(env, objectHandles[i]);
    }

    ERL_NIF_TERM handlesList = enif_make_list_from_array(env, objectHandlesArray,
        objectCount);

    free(objectHandlesArray);
    return enif_make_tuple2(env, enif_make_int(env, ret), handlesList);
}

ERL_FUNC(getObjectSelection) {
    PARAM(int, 0, clientID);
    OPMODE(1, operationMode);
    int objectCount;
    int *objectHandles;

    int ret = simxGetObjectSelection(clientID, &objectHandles, &objectCount,
        operationMode);

    ERL_NIF_TERM* objectHandlesArray = malloc(sizeof(ERL_NIF_TERM)*objectCount);

    int i = 0;
    for (; i < objectCount; ++i) {
        objectHandlesArray[i] = enif_make_int(env, objectHandles[i]);
    }

    ERL_NIF_TERM handlesList = enif_make_list_from_array(env, objectHandlesArray,
        objectCount);

    free(objectHandlesArray);
    return enif_make_tuple2(env, enif_make_int(env, ret), handlesList);
}

ERL_FUNC(getObjectVelocity) {
    PARAM(int, 0, clientID);
    PARAM(int, 1, objectHandle);
    OPMODE(2, operationMode);
    float linearVelocity[3], angularVelocity[3];

    int ret = simxGetObjectVelocity(clientID, objectHandle, linearVelocity,
        angularVelocity, operationMode);
    return enif_make_tuple3(env, enif_make_int(env, ret), enif_make_tuple3(env,
        enif_make_double(env, (double)linearVelocity[0]),
        enif_make_double(env, (double)linearVelocity[1]),
        enif_make_double(env, (double)linearVelocity[2])),
        enif_make_tuple3(env,
            enif_make_double(env, (double)angularVelocity[0]),
            enif_make_double(env, (double)angularVelocity[1]),
            enif_make_double(env, (double)angularVelocity[2])));
}

ERL_FUNC(getOutMessageInfo) {
    PARAM(int, 0, clientID);
    PARAM(int, 1, infoType);
    int info;

    int ret = simxGetOutMessageInfo(clientID, infoType, &info);
    return enif_make_tuple2(env, enif_make_int(env, ret), enif_make_int(env,
        info));
}

ERL_FUNC(getPingTime) {
    PARAM(int, 0, clientID);
    int pingTime;

    int ret = simxGetPingTime(clientID, &pingTime);
    return enif_make_tuple2(env, enif_make_int(env, ret), enif_make_int(env,
        pingTime));
}

ERL_FUNC(getStringParameter) {
    PARAM(int, 0, clientID);
    PARAM(int, 1, paramIdentifier);
    OPMODE(2, operationMode);
    char *paramValue;

    int ret = simxGetStringParameter(clientID, paramIdentifier, &paramValue,
        operationMode);
    return enif_make_tuple2(env, enif_make_int(env, ret), enif_make_string(env,
        paramValue, ERL_NIF_LATIN1));
}

ERL_FUNC(getStringSignal) {
    PARAM(int, 0, clientID);
    SPARAM(1, signalName);
    OPMODE(2, operationMode);
    int signalLength;
    char *signalValue;

    int ret = simxGetStringSignal(clientID, signalName, &signalValue,
        &signalLength, operationMode);
    return enif_make_tuple2(env, enif_make_int(env, ret),
        enif_make_string_len(env, signalValue, (size_t)signalLength, ERL_NIF_LATIN1));
}

ERL_FUNC(getUIButtonProperty) {
    PARAM(int, 0, clientID);
    PARAM(int, 1, uiHandle);
    PARAM(int, 2, uiButtonID);
    OPMODE(3, operationMode);
    int prop;

    int ret = simxGetUIButtonProperty(clientID, uiHandle, uiButtonID, &prop,
        operationMode);
    return enif_make_tuple2(env, enif_make_int(env, ret), enif_make_int(env,
        prop));
}

ERL_FUNC(getUIEventButton) {
    PARAM(int, 0, clientID);
    PARAM(int, 1, uiHandle);
    PARAM(int, 2, uiEventButtonID);
    OPMODE(3, operationMode);
    int auxValues[2];

    int ret = simxGetUIEventButton(clientID, uiHandle, &uiEventButtonID,
        auxValues, operationMode);
    return enif_make_tuple3(env, enif_make_int(env, ret), enif_make_int(env,
        uiEventButtonID), enif_make_tuple2(env, enif_make_int(env, auxValues[0]),
        enif_make_int(env, auxValues[1])));
}

ERL_FUNC(getUIHandle) {
    PARAM(int, 0, clientID);
    SPARAM(1, uiName);
    OPMODE(2, operationMode);
    int handle;

    int ret = simxGetUIHandle(clientID, uiName, &handle, operationMode);
    return enif_make_tuple2(env, enif_make_int(env, ret), enif_make_int(env,
        handle));
}

ERL_FUNC(getUISlider) {
    PARAM(int, 0, clientID);
    PARAM(int, 1, uiHandle);
    PARAM(int, 2, uiButtonID);
    OPMODE(3, operationMode);
    int position;

    int ret = simxGetUISlider(clientID, uiHandle, uiButtonID, &position,
        operationMode);
    return enif_make_tuple2(env, enif_make_int(env, ret), enif_make_int(env,
        position));
}

ERL_FUNC(getVisionSensorDepthBuffer) {
    PARAM(int, 0, clientID);
    PARAM(int, 1, sensorHandle);
    OPMODE(2, operationMode);
    int resolution[2];
    float *buffer;

    int ret = simxGetVisionSensorDepthBuffer(clientID, sensorHandle,
        resolution, &buffer, operationMode);
    // TODO convert buffer to something more usable
    return enif_make_int(env, ret);
}

ERL_FUNC(getVisionSensorImage) {
    PARAM(int, 0, clientID);
    PARAM(int, 1, sensorHandle);
    PARAM(int, 2, options);
    OPMODE(3, operationMode);
    int resolution[2];
    char* image;

    int ret = simxGetVisionSensorImage(clientID, sensorHandle, resolution,
        &image, (char)options, operationMode);
    // TODO convert image to something
    return enif_make_int(env, ret);
}

ERL_FUNC(jointGetForce) {
    PARAM(int, 0, clientID);
    PARAM(int, 1, jointHandle);
    OPMODE(2, operationMode);
    float force;

    int ret = simxJointGetForce(clientID, jointHandle, &force, operationMode);
    return enif_make_tuple2(env, enif_make_int(env, ret), enif_make_double(env,
        (double)force));
}

ERL_FUNC(loadModel) {
    PARAM(int, 0, clientID);
    SPARAM(1, modelPathAndName);
    PARAM(int, 2, options);
    OPMODE(3, operationMode);
    int baseHandle;

    int ret = simxLoadModel(clientID, modelPathAndName, (char)options,
        &baseHandle, operationMode);
    return enif_make_tuple2(env, enif_make_int(env, ret), enif_make_int(env,
        baseHandle));
}

ERL_FUNC(loadScene) {
    PARAM(int, 0, clientID);
    SPARAM(1, scenePathAndName);
    PARAM(int, 2, options);
    OPMODE(3, operationMode);

    int ret = simxLoadScene(clientID, scenePathAndName, (char)options, operationMode);
    return enif_make_int(env, ret);
}

ERL_FUNC(loadUI) {
    PARAM(int, 0, clientID);
    SPARAM(1, uiPathAndName);
    PARAM(int, 2, options);
    OPMODE(3, operationMode);
    int count;
    int *uiHandles;

    int ret = simxLoadUI(clientID, uiPathAndName, (char)options, &count,
        &uiHandles, operationMode);

    ERL_NIF_TERM* handlesArray = malloc(sizeof(ERL_NIF_TERM)*count);

    int i = 0;
    for (; i < count; ++i) {
        handlesArray[i] = enif_make_int(env, uiHandles[i]);
    }

    ERL_NIF_TERM handlesList = enif_make_list_from_array(env, handlesArray, count);

    free(handlesArray);
    simxReleaseBuffer((char*)uiHandles);
    return enif_make_tuple2(env, enif_make_int(env, ret), handlesList);
}

ERL_FUNC(pauseCommunication) {
    PARAM(int, 0, clientID);
    PARAM(int, 1, pause);

    int ret = simxPauseCommunication(clientID, pause);
    return enif_make_int(env, ret);
}

ERL_FUNC(pauseSimulation) {
    PARAM(int, 0, clientID);
    OPMODE(1, operationMode);

    int ret = simxPauseSimulation(clientID, operationMode);
    return enif_make_int(env, ret);
}

ERL_FUNC(query) {
    PARAM(int, 0, clientID);
    SPARAM(1, signalName);
    SPARAM(2, signalValue);
    PARAM(int, 3, signalLength);
    SPARAM(4, retSignalName);
    PARAM(int, 5, timeOutInMs);
    int retSignalLength;
    char *retSignalValue;

    int ret = simxQuery(clientID, signalName, signalValue, signalLength,
        retSignalName, &retSignalValue, &retSignalLength, timeOutInMs);
    return enif_make_tuple2(env, enif_make_int(env, ret), enif_make_string_len(
        env, retSignalValue, (size_t)256, ERL_NIF_LATIN1));
}

ERL_FUNC(readCollision) {
    PARAM(int, 0, clientID);
    PARAM(int, 1, collisionObjectHandle);
    OPMODE(2, operationMode);
    char collisionState;

    int ret = simxReadCollision(clientID, collisionObjectHandle, &collisionState,
        operationMode);
    return enif_make_tuple2(env, enif_make_int(env, ret), enif_make_int(env,
        (int)collisionState));
}

ERL_FUNC(readDistance) {
    PARAM(int, 0, clientID);
    PARAM(int, 1, distanceObjectHandle);
    OPMODE(2, operationMode);
    float minimumDistance;

    int ret = simxReadDistance(clientID, distanceObjectHandle, &minimumDistance,
        operationMode);
    return enif_make_tuple2(env, enif_make_int(env, ret), enif_make_double(env,
        (double)minimumDistance));
}

ERL_FUNC(readForceSensor) {
    PARAM(int, 0, clientID);
    PARAM(int, 1, forceSensorHandle);
    OPMODE(2, operationMode);
    char state;
    float forceVector[3], torqueVector[3];

    int ret = simxReadForceSensor(clientID, forceSensorHandle, &state,
        forceVector, torqueVector, operationMode);
    return enif_make_tuple3(env, enif_make_int(env, ret), enif_make_tuple3(env,
        enif_make_double(env, (double)forceVector[0]),
        enif_make_double(env, (double)forceVector[1]),
        enif_make_double(env, (double)forceVector[2])),
        enif_make_tuple3(env,
        enif_make_double(env, (double)torqueVector[0]),
        enif_make_double(env, (double)torqueVector[1]),
        enif_make_double(env, (double)torqueVector[2])));
}

ERL_FUNC(readProximitySensor) {
    PARAM(int, 0, clientID);
    PARAM(int, 1, sensorHandle);
    OPMODE(2, operationMode);
    int detectedObjectHandle;
    char detectionState;
    float detectedPoint[3], detectedSurfaceNormalVector[3];

    int ret = simxReadProximitySensor(clientID, sensorHandle, &detectionState,
        detectedPoint, &detectedObjectHandle, detectedSurfaceNormalVector,
        operationMode);
    // {ret, detectionState, {{point[0], point[1], point[2]}, detectedObjectHandle,
    // {vector[0], vector[1], vector[2]}}}
    return enif_make_tuple3(env, enif_make_int(env, ret), enif_make_int(env,
        (int)detectionState), enif_make_tuple3(env, enif_make_tuple3(env,
        enif_make_double(env, (double)detectedPoint[0]),
        enif_make_double(env, (double)detectedPoint[1]),
        enif_make_double(env, (double)detectedPoint[2])),
        enif_make_int(env, detectedObjectHandle),
        enif_make_tuple3(env,
        enif_make_double(env, (double)detectedSurfaceNormalVector[0]),
        enif_make_double(env, (double)detectedSurfaceNormalVector[1]),
        enif_make_double(env, (double)detectedSurfaceNormalVector[2]))));
}

ERL_FUNC(readVisionSensor) {
    PARAM(int, 0, clientID);
    PARAM(int, 1, sensorHandle);
    OPMODE(2, operationMode);
    char detectionState;
    float *auxValues;
    int *auxValuesCount;

    int ret = simxReadVisionSensor(clientID, sensorHandle, &detectionState,
        &auxValues, &auxValuesCount, operationMode);
    // TODO return something useful

    simxReleaseBuffer((char*)auxValues);
    simxReleaseBuffer((char*)auxValuesCount);
    return enif_make_int(env, ret);
}

// simxReleaseBuffer isn't really useful in Erlang, is it?

ERL_FUNC(removeObject) {
    PARAM(int, 0, clientID);
    PARAM(int, 1, objectHandle);
    OPMODE(2, operationMode);

    int ret = simxRemoveObject(clientID, objectHandle, operationMode);
    return enif_make_int(env, ret);
}

ERL_FUNC(removeUI) {
    PARAM(int, 0, clientID);
    PARAM(int, 1, uiHandle);
    OPMODE(2, operationMode);

    int ret = simxRemoveUI(clientID, uiHandle, operationMode);
    return enif_make_int(env, ret);
}

ERL_FUNC(setArrayParameter) {
    PARAM(int, 0, clientID);
    PARAM(int, 1, paramIdentifier);
    TUPLE(double, 2, paramValues, 3);
    OPMODE(3, operationMode);

    D2F(paramValues, fParamValues, 3);

    int ret = simxSetArrayParameter(clientID, paramIdentifier, fParamValues,
        operationMode);
    return enif_make_int(env, ret);
}

ERL_FUNC(setBooleanParameter) {
    PARAM(int, 0, clientID);
    APARAM(1, paramIdentifier);
    PARAM(int, 2, paramValue);
    OPMODE(3, operationMode);

    int boolParamIdentifier = boolParamAtomToInt(paramIdentifier);
    if (boolParamIdentifier == -1)
        return enif_make_badarg(env);

    int ret = simxSetBooleanParameter(clientID, boolParamIdentifier,
        (char)paramValue, operationMode);
    return enif_make_int(env, ret);
}

ERL_FUNC(setFloatingParameter) {
    PARAM(int, 0, clientID);
    APARAM(1, paramIdentifier);
    PARAM(double, 2, paramValue);
    OPMODE(3, operationMode);

    int floatParamIdentifier = floatParamAtomToInt(paramIdentifier);
    if (floatParamIdentifier == -1)
        return enif_make_badarg(env);

    int ret = simxSetFloatingParameter(clientID, floatParamIdentifier,
        (float)paramValue, operationMode);
    return enif_make_int(env, ret);
}

ERL_FUNC(setFloatSignal) {
    PARAM(int, 0, clientID);
    SPARAM(1, signalName);
    PARAM(double, 2, signalValue);
    OPMODE(3, operationMode);

    int ret = simxSetFloatSignal(clientID, signalName, (float)signalValue,
        operationMode);
    return enif_make_int(env, ret);
}

ERL_FUNC(setIntegerParameter) {
    PARAM(int, 0, clientID);
    APARAM(1, paramIdentifier);
    PARAM(int, 2, paramValue);
    OPMODE(3, operationMode);

    int intParamIdentifier = integerParamAtomToInt(paramIdentifier);
    if (intParamIdentifier == -1)
        return enif_make_badarg(env);

    int ret = simxSetIntegerParameter(clientID, intParamIdentifier, paramValue,
        operationMode);
    return enif_make_int(env, ret);
}

ERL_FUNC(setIntegerSignal) {
    PARAM(int, 0, clientID);
    SPARAM(1, signalName);
    PARAM(int, 2, signalValue);
    OPMODE(3, operationMode);

    int ret = simxSetIntegerSignal(clientID, signalName, signalValue,
        operationMode);
    return enif_make_int(env, ret);
}

ERL_FUNC(setJointForce) {
    PARAM(int, 0, clientID);
    PARAM(int, 1, jointHandle);
    PARAM(double, 2, force);
    OPMODE(3, operationMode);

    int ret = simxSetJointForce(clientID, jointHandle, (float)force,
        operationMode);
    return enif_make_int(env, ret);
}

ERL_FUNC(setJointPosition) {
    PARAM(int, 0, clientID);
    PARAM(int, 1, jointHandle);
    PARAM(double, 2, position);
    OPMODE(3, operationMode);

    int ret = simxSetJointPosition(clientID, jointHandle, (float)position,
        operationMode);
    return enif_make_int(env, ret);
}

ERL_FUNC(setJointTargetPosition) {
    PARAM(int, 0, clientID);
    PARAM(int, 1, jointHandle);
    PARAM(double, 2, targetPosition);
    OPMODE(3, operationMode);

    int ret = simxSetJointTargetPosition(clientID, jointHandle, (float)targetPosition,
        operationMode);
    return enif_make_int(env, ret);
}

ERL_FUNC(setJointTargetVelocity) {
    PARAM(int, 0, clientID);
    PARAM(int, 1, jointHandle);
    PARAM(double, 2, targetVelocity);
    OPMODE(3, operationMode);

    int ret = simxSetJointTargetVelocity(clientID, jointHandle,
        (float)targetVelocity, operationMode);
    return enif_make_int(env, ret);
}

ERL_FUNC(setModelProperty) {
    PARAM(int, 0, clientID);
    PARAM(int, 1, objectHandle);
    APARAM(2, prop);
    OPMODE(3, operationMode);

    int intProp = modelPropAtomToInt(prop);
    if (intProp == -1)
        return enif_make_badarg(env);

    int ret = simxSetModelProperty(clientID, objectHandle, intProp, operationMode);
    return enif_make_int(env, ret);
}

ERL_FUNC(setObjectFloatParameter) {
    PARAM(int, 0, clientID);
    PARAM(int, 1, objectHandle);
    PARAM(int, 2, parameterID);
    PARAM(double, 3, parameterValue);
    OPMODE(4, operationMode);

    int ret = simxSetObjectFloatParameter(clientID, objectHandle, parameterID,
        (float)parameterValue, operationMode);
    return enif_make_int(env, ret);
}

ERL_FUNC(setObjectIntParameter) {
    PARAM(int, 0, clientID);
    PARAM(int, 1, objectHandle);
    PARAM(int, 2, parameterID);
    PARAM(int, 3, parameterValue);
    OPMODE(4, operationMode);

    int ret = simxSetObjectIntParameter(clientID, objectHandle, parameterID,
        parameterValue, operationMode);
    return enif_make_int(env, ret);
}

ERL_FUNC(setObjectOrientation) {
    PARAM(int, 0, clientID);
    PARAM(int, 1, objectHandle);
    PARAM(int, 2, relativeToObjectHandle);
    TUPLE(double, 3, eulerAngles, 3);
    OPMODE(4, operationMode);

    D2F(eulerAngles, fEulerAngles, 3);

    int ret = simxSetObjectOrientation(clientID, objectHandle, relativeToObjectHandle,
        fEulerAngles, operationMode);
    return enif_make_int(env, ret);
}

ERL_FUNC(setObjectParent) {
    PARAM(int, 0, clientID);
    PARAM(int, 1, objectHandle);
    PARAM(int, 2, parentObject);
    PARAM(int, 3, keepInPlace);
    OPMODE(4, operationMode);

    int ret = simxSetObjectParent(clientID, objectHandle, parentObject,
        keepInPlace, operationMode);
    return enif_make_int(env, ret);
}

ERL_FUNC(setObjectPosition) {
    PARAM(int, 0, clientID);
    PARAM(int, 1, objectHandle);
    PARAM(int, 2, relativeToObjectHandle);
    TUPLE(double, 3, position, 3);
    OPMODE(4, operationMode);

    D2F(position, fPosition, 3);

    int ret = simxSetObjectPosition(clientID, objectHandle, relativeToObjectHandle,
        fPosition, operationMode);
    return enif_make_int(env, ret);
}

ERL_FUNC(setObjectSelection) {
    PARAM(int, 0, clientID);
    LIST(int, 1, objectHandles, objectHandlesL);
    OPMODE(2, operationMode);

    int ret = simxSetObjectSelection(clientID, objectHandles, objectHandlesL,
        operationMode);

    free(objectHandles);
    return enif_make_int(env, ret);
}

ERL_FUNC(setSphericalJointMatrix) {
    PARAM(int, 0, clientID);
    PARAM(int, 1, jointHandle);
    TUPLE(double, 2, matrix, 12);
    OPMODE(3, operationMode);

    D2F(matrix, fMatrix, 12);

    int ret = simxSetSphericalJointMatrix(clientID, jointHandle, fMatrix,
        operationMode);
    return enif_make_int(env, ret);
}

ERL_FUNC(setStringSignal) {
    PARAM(int, 0, clientID);
    SPARAM(1, signalName);
    SPARAM(2, signalValue);
    PARAM(int, 3, signalLength);
    OPMODE(4, operationMode);
    // TODO
    // will work with '\0's embedded in value?
    int ret = simxSetStringSignal(clientID, signalName, signalValue, signalLength,
        operationMode);
    return enif_make_int(env, ret);
}

ERL_FUNC(setUIButtonLabel) {
    PARAM(int, 0, clientID);
    PARAM(int, 1, uiHandle);
    PARAM(int, 2, uiButtonID);
    SPARAM(3, upStateLabel);
    SPARAM(4, downStateLabel);
    OPMODE(5, operationMode);

    int ret = simxSetUIButtonLabel(clientID, uiHandle, uiButtonID, upStateLabel,
        downStateLabel, operationMode);
    return enif_make_int(env, ret);
}


ERL_FUNC(setUIButtonProperty) {
    PARAM(int, 0, clientID);
    PARAM(int, 1, uiHandle);
    PARAM(int, 2, uiButtonID);
    PARAM(int, 3, prop);
    OPMODE(4, operationMode);

    int ret = simxSetUIButtonProperty(clientID, uiHandle, uiButtonID, prop,
        operationMode);
    return enif_make_int(env, ret);
}

ERL_FUNC(setUISlider) {
    PARAM(int, 0, clientID);
    PARAM(int, 1, uiHandle);
    PARAM(int, 2, uiButtonID);
    PARAM(int, 3, position);
    OPMODE(4, operationMode);

    int ret = simxSetUISlider(clientID, uiHandle, uiButtonID, position,
        operationMode);
    return enif_make_int(env, ret);
}

ERL_FUNC(setVisionSensorImage) {
    PARAM(int, 0, clientID);
    PARAM(int, 1, sensorHandle);
    char* image = NULL;
    // stub image for now, should receive binary data?
    PARAM(int, 3, bufferSize);
    PARAM(int, 4, options);
    OPMODE(5, operationMode);

    int ret = simxSetVisionSensorImage(clientID, sensorHandle, image,
        bufferSize, (char)options, operationMode);
    return enif_make_int(env, ret);
}

ERL_FUNC(startSimulation) {
    PARAM(int, 0, clientID);
    OPMODE(1, operationMode);

    int ret = simxStartSimulation(clientID, operationMode);
    return enif_make_int(env, ret);
}

ERL_FUNC(stopSimulation) {
    PARAM(int, 0, clientID);
    OPMODE(1, operationMode);

    int ret = simxStopSimulation(clientID, operationMode);
    return enif_make_int(env, ret);
}

ERL_FUNC(synchronous) {
    PARAM(int, 0, clientID);
    PARAM(int, 1, enable);

    int ret = simxSynchronous(clientID, (char)enable);
    return enif_make_int(env, ret);
}

ERL_FUNC(synchronousTrigger) {
    PARAM(int, 0, clientID);

    int ret = simxSynchronousTrigger(clientID);
    return enif_make_int(env, ret);
}

ERL_FUNC(transferFile) {
    PARAM(int, 0, clientID);
    SPARAM(1, filePathAndName);
    SPARAM(2, fileName_serverSide);
    PARAM(int, 3, timeOut);
    OPMODE(4, operationMode);

    int ret = simxTransferFile(clientID, filePathAndName, fileName_serverSide,
        timeOut, operationMode);
    return enif_make_int(env, ret);
}

static ErlNifFunc functions[] = {
    {"start", 6, start},
    {"finish", 1, finish},
    {"addStatusbarMessage", 3, addStatusbarMessage},
    {"appendStringSignal", 5, appendStringSignal},
    {"auxiliaryConsoleClose", 3, auxiliaryConsoleClose},
    {"auxiliaryConsoleOpen", 9, auxiliaryConsoleOpen},
    {"auxiliaryConsolePrint", 4, auxiliaryConsolePrint},
    {"auxiliaryConsoleShow", 4, auxiliaryConsoleShow},
    {"breakForceSensor", 3, breakForceSensor},
    {"clearFloatSignal", 3, clearFloatSignal},
    {"clearIntegerSignal", 3, clearIntegerSignal},
    {"clearStringSignal", 3, clearStringSignal},
    {"closeScene", 2, closeScene},
    {"copyPasteObjects", 3, copyPasteObjects},
    {"createDummy", 3, createDummy},
    {"displayDialog", 8, displayDialog},
    {"endDialog", 3, endDialog},
    {"eraseFile", 3, eraseFile},
    {"getAndClearStringSignal", 3, getAndClearStringSignal},
    {"getArrayParameter", 3, getArrayParameter},
    {"getBooleanParameter", 3, getBooleanParameter},
    {"getCollisionHandle", 3, getCollisionHandle},
    {"getConnectionId", 1, getConnectionId},
    {"getDialogInput", 3, getDialogInput},
    {"getDialogResult", 3, getDialogResult},
    {"getDistanceHandle", 3, getDistanceHandle},
    {"getFloatingParameter", 3, getFloatingParameter},
    {"getFloatSignal", 3, getFloatSignal},
    {"getInMessageInfo", 2, getInMessageInfo},
    {"getIntegerParameter", 3, getIntegerParameter},
    {"getIntegerSignal", 3, getIntegerSignal},
    {"getJointMatrix", 3, getJointMatrix},
    {"getJointPosition", 3, getJointPosition},
    {"getLastCmdTime", 1, getLastCmdTime},
    {"getLastErrors", 2, getLastErrors},
    {"getModelProperty", 3, getModelProperty},
    {"getObjectChild", 4, getObjectChild},
    {"getObjectFloatParameter", 4, getObjectFloatParameter},
    {"getObjectGroupData", 4, getObjectGroupData},
    {"getObjectHandle", 3, getObjectHandle},
    {"getObjectIntParameter", 4, getObjectIntParameter},
    {"getObjectOrientation", 4, getObjectOrientation},
    {"getObjectParent", 3, getObjectParent},
    {"getObjectPosition", 4, getObjectPosition},
    {"getObjects", 3, getObjects},
    {"getObjectSelection", 2, getObjectSelection},
    {"getObjectVelocity", 3, getObjectVelocity},
    {"getOutMessageInfo", 2, getOutMessageInfo},
    {"getPingTime", 1, getPingTime},
    {"getStringParameter", 3, getStringParameter},
    {"getStringSignal", 3, getStringSignal},
    {"getUIButtonProperty", 4, getUIButtonProperty},
    {"getUIEventButton", 4, getUIEventButton},
    {"getUIHandle", 3, getUIHandle},
    {"getUISlider", 4, getUISlider},
    {"getVisionSensorDepthBuffer", 3, getVisionSensorDepthBuffer},
    {"getVisionSensorImage", 4, getVisionSensorImage},
    {"jointGetForce", 3, jointGetForce},
    {"loadModel", 4, loadModel},
    {"loadScene", 4, loadScene},
    {"loadUI", 4, loadUI},
    {"pauseCommunication", 2, pauseCommunication},
    {"pauseSimulation", 2, pauseSimulation},
    {"query", 6, query},
    {"readCollision", 3, readCollision},
    {"readDistance", 3, readDistance},
    {"readForceSensor", 3, readForceSensor},
    {"readProximitySensor", 3, readProximitySensor},
    {"readVisionSensor", 3, readVisionSensor},
    {"removeObject", 3, removeObject},
    {"removeUI", 3, removeUI},
    {"setArrayParameter", 4, setArrayParameter},
    {"setBooleanParameter", 4, setBooleanParameter},
    {"setFloatingParameter", 4, setFloatingParameter},
    {"setFloatSignal", 4, setFloatSignal},
    {"setIntegerParameter", 4, setIntegerParameter},
    {"setIntegerSignal", 4, setIntegerSignal},
    {"setJointForce", 4, setJointForce},
    {"setJointPosition", 4, setJointPosition},
    {"setJointTargetPosition", 4, setJointTargetPosition},
    {"setJointTargetVelocity", 4, setJointTargetVelocity},
    {"setModelProperty", 4, setModelProperty},
    {"setObjectFloatParameter", 5, setObjectFloatParameter},
    {"setObjectIntParameter", 5, setObjectIntParameter},
    {"setObjectOrientation", 5, setObjectOrientation},
    {"setObjectParent", 5, setObjectParent},
    {"setObjectPosition", 5, setObjectPosition},
    {"setObjectSelection", 3, setObjectSelection},
    {"setSphericalJointMatrix", 4, setSphericalJointMatrix},
    {"setStringSignal", 5, setStringSignal},
    {"setUIButtonLabel", 6, setUIButtonLabel},
    {"setUIButtonProperty", 5, setUIButtonProperty},
    {"setUISlider", 5, setUISlider},
    {"setVisionSensorImage", 6, setVisionSensorImage},
    {"startSimulation", 2, startSimulation},
    {"stopSimulation", 2, stopSimulation},
    {"synchronous", 2, synchronous},
    {"synchronousTrigger", 1, synchronousTrigger},
    {"transferFile", 5, transferFile}
};

ERL_NIF_INIT(simx, functions, NULL, NULL, NULL, NULL)