
require "Cocos2d"

-- cclog
local cclog = function(...)
    print(string.format(...))
end

-- for CCLuaEngine traceback
function __G__TRACKBACK__(msg)
    cclog("----------------------------------------")
    cclog("LUA ERROR: " .. tostring(msg) .. "\n")
    cclog(debug.traceback())
    cclog("----------------------------------------")
    return msg
end


-- WebSockets
local function wsSendTextOpen(strData)
    cclog("| WS Opened")
    sendTextState:setString("Send Text WS was opened")
end
local function wsSendTextMessage(strData)
    cclog("| Send Msg")
    receiveTextTimes=receiveTextTimes+1
    local strInfo="response text msg: "..strData..","..receiveTextTimes
    sendTextStatus:setString(strInfo)
end
local function wsSendTextClose(strData)
    cclog("| Send close")
    print("_wsiSendText websocket instance closed")
    sendTextStatus=nil
    wsSendText=nil
end
local function wsSendTextError(strData)
    cclog("| Send Error")
    print("sendText Error was fired")
end
-----

local function main()
    collectgarbage("collect")
    -- avoid memory leak
    collectgarbage("setpause", 100)
    collectgarbage("setstepmul", 5000)
    
    cc.FileUtils:getInstance():addSearchPath("src")
    cc.FileUtils:getInstance():addSearchPath("res")
    cc.Director:getInstance():getOpenGLView():setDesignResolutionSize(480, 320, 0)
    
    -- WebSockets
    cclog("| Creating ws")
    wsSendText = WebSocket:create("ws://zen:8081")
    if nil ~= wsSendText then
        wsSendText:registerScriptHandler(wsSendTextOpen,cc.WEBSOCKET_OPEN)
        wsSendText:registerScriptHandler(wsSendTextMessage,cc.WEBSOCKET_MESSAGE)
        wsSendText:registerScriptHandler(wsSendTextClose,cc.WEBSOCKET_CLOSE)
        wsSendText:registerScriptHandler(wsSendTextError,cc.WEBSOCKET_ERROR)
    end
    cclog("| Sending msg")
    wsSendText:sendString("Hello World")
    cclog("| Msg sent")
    wsSendText:close()
    
    --create scene 
    local scene = require("GameScene")
    local gameScene = scene.create()
    gameScene:playBgMusic()
    
    if cc.Director:getInstance():getRunningScene() then
        cc.Director:getInstance():replaceScene(gameScene)
    else
        cc.Director:getInstance():runWithScene(gameScene)
    end

end

local status, msg = xpcall(main, __G__TRACKBACK__)
if not status then
    error(msg)
end
