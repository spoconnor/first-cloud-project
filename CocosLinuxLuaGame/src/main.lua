require "Cocos2d"
require "Cocos2dConstants"
require "NetworkConstants"

-- cclog
cclog = function(...)
    print(string.format(...))
end


--for CCLuaEngine traceback
function __G__TRACKBACK__(msg)
    cclog("----------------------------------------")
    cclog("LUA ERROR: " .. tostring(msg) .. "\n")
    cclog(debug.traceback())
    cclog("----------------------------------------")
    return msg
end

local function main()

    local size = cc.Director:getInstance():getWinSize()
    local scheduler = cc.Director:getInstance():getScheduler()
    local kTagTileMap = 1
    local s_TilesPng = "tiles.png"
    local s_LevelMapTga = "levelmap.tga"

    collectgarbage("collect")
    -- avoid memory leak
    collectgarbage("setpause", 100)
    collectgarbage("setstepmul", 5000)

    -- initialize director
    local director = cc.Director:getInstance()
    local glview = director:getOpenGLView()
    if nil == glview then
        glview = cc.GLViewImpl:createWithRect("HelloLua", cc.rect(0,0,900,640))
        director:setOpenGLView(glview)
    end

    glview:setDesignResolutionSize(480, 320, cc.ResolutionPolicy.NO_BORDER)


    --turn on display FPS
    director:setDisplayStats(true)

    --set FPS. the default value is 1.0/60 if you don't call this
    director:setAnimationInterval(1.0 / 60)

	cc.FileUtils:getInstance():addSearchPath("src")
	cc.FileUtils:getInstance():addSearchPath("res")
	local schedulerID = 0
    --support debug
    local targetPlatform = cc.Application:getInstance():getTargetPlatform()
    if (cc.PLATFORM_OS_IPHONE == targetPlatform) or (cc.PLATFORM_OS_IPAD == targetPlatform) or 
       (cc.PLATFORM_OS_ANDROID == targetPlatform) or (cc.PLATFORM_OS_WINDOWS == targetPlatform) or
       (cc.PLATFORM_OS_MAC == targetPlatform) then
        cclog("result is ")
		--require('debugger')()
        
    end
--    require "hello2"
--    cclog("result is " .. myadd(1, 1))

--wsSendText  = cc.WebSocket:create("ws://127.0.0.1:8081")

    ---------------

    local visibleSize = cc.Director:getInstance():getVisibleSize()
    local origin = cc.Director:getInstance():getVisibleOrigin()

--    -- add the moving dog
--    local function createDog()
--        local frameWidth = 105
--        local frameHeight = 95
--
--        -- create dog animate
--        local textureDog = cc.Director:getInstance():getTextureCache():addImage("dog.png")
--        local rect = cc.rect(0, 0, frameWidth, frameHeight)
--        local frame0 = cc.SpriteFrame:createWithTexture(textureDog, rect)
--        rect = cc.rect(frameWidth, 0, frameWidth, frameHeight)
--        local frame1 = cc.SpriteFrame:createWithTexture(textureDog, rect)
--
--        local spriteDog = cc.Sprite:createWithSpriteFrame(frame0)
--        spriteDog.isPaused = false
--        spriteDog:setPosition(origin.x, origin.y + visibleSize.height / 4 * 3)
----[[
--        local animFrames = CCArray:create()
--
--        animFrames:addObject(frame0)
--        animFrames:addObject(frame1)
--]]--
--
--        local animation = cc.Animation:createWithSpriteFrames({frame0,frame1}, 0.5)
--        local animate = cc.Animate:create(animation);
--        spriteDog:runAction(cc.RepeatForever:create(animate))
--
--        -- moving dog at every frame
--        local function tick()
--            if spriteDog.isPaused then return end
--            local x, y = spriteDog:getPosition()
--            if x > origin.x + visibleSize.width then
--                x = origin.x
--            else
--                x = x + 1
--            end
--
--            spriteDog:setPositionX(x)
--        end
--
--        schedulerID = cc.Director:getInstance():getScheduler():scheduleScriptFunc(tick, 0, false)
--
--        return spriteDog
--    end

--    -- create farm
--    local function createLayerFarm()
--        local layerFarm = cc.Layer:create()
--
--        -- add in farm background
--        local bg = cc.Sprite:create("farm.jpg")
--        bg:setPosition(origin.x + visibleSize.width / 2 + 80, origin.y + visibleSize.height / 2)
--        layerFarm:addChild(bg)
--
--        -- add land sprite
--        for i = 0, 3 do
--            for j = 0, 1 do
--                local spriteLand = cc.Sprite:create("land.png")
--                spriteLand:setPosition(200 + j * 180 - i % 2 * 90, 10 + i * 95 / 2)
--                layerFarm:addChild(spriteLand)
--            end
--        end
--
--        -- add crop
--        local frameCrop = cc.SpriteFrame:create("crop.png", cc.rect(0, 0, 105, 95))
--        for i = 0, 3 do
--            for j = 0, 1 do
--                local spriteCrop = cc.Sprite:createWithSpriteFrame(frameCrop);
--                spriteCrop:setPosition(10 + 200 + j * 180 - i % 2 * 90, 30 + 10 + i * 95 / 2)
--                layerFarm:addChild(spriteCrop)
--            end
--        end
--
--        -- add moving dog
--        local spriteDog = createDog()
--        layerFarm:addChild(spriteDog)
--
--        local function onNodeEvent(event)
--           if "exit" == event then
--               cc.Director:getInstance():getScheduler():unscheduleScriptEntry(schedulerID)
--           end
--        end
--        layerFarm:registerScriptHandler(onNodeEvent)
--
--        return layerFarm
--    end


    local function TileMapEditTest()
        cclog("TileMapEditTest")
        local layerTiles = cc.Layer:create()
        -- handing touch events
        local function onTouchesMoved(touches, event )
            local diff = touches[1]:getDelta()
            local node = layerTiles:getChildByTag(kTagTileMap)
            local currentPosX, currentPosY= node:getPosition()
            node:setPosition(cc.p(currentPosX + diff.x, currentPosY + diff.y))
        end

--        local touchBeginPoint = nil
--        local function onTouchBegan(touch, event)
--            local location = touch:getLocation()
--            cclog("onTouchBegan: %0.2f, %0.2f", location.x, location.y)
--            touchBeginPoint = {x = location.x, y = location.y}
--            spriteDog.isPaused = true
--            -- CCTOUCHBEGAN event must return true
--            return true
--        end

--        local function onTouchMoved(touch, event)
--            local location = touch:getLocation()
--            cclog("onTouchMoved: %0.2f, %0.2f", location.x, location.y)
--            if touchBeginPoint then
--                local cx, cy = layerTiles:getPosition()
--                layerTiles:setPosition(cx + location.x - touchBeginPoint.x,
--                    cy + location.y - touchBeginPoint.y)
--                touchBeginPoint = {x = location.x, y = location.y}
--            end
--        end

--        local function onTouchEnded(touch, event)
--            local location = touch:getLocation()
--            cclog("onTouchEnded: %0.2f, %0.2f", location.x, location.y)
--            touchBeginPoint = nil
--            spriteDog.isPaused = false
--        end

        --        local listener = cc.EventListenerTouchOneByOne:create()
        --        listener:registerScriptHandler(onTouchBegan,cc.Handler.EVENT_TOUCH_BEGAN )
        --        listener:registerScriptHandler(onTouchMoved,cc.Handler.EVENT_TOUCH_MOVED )
        --        listener:registerScriptHandler(onTouchEnded,cc.Handler.EVENT_TOUCH_ENDED )
        --        local eventDispatcher = layerTiles:getEventDispatcher()
        --        eventDispatcher:addEventListenerWithSceneGraphPriority(listener, layerTiles)

        cclog("TileMapEditTest events")
        local listener = cc.EventListenerTouchAllAtOnce:create()
        listener:registerScriptHandler(onTouchesMoved,cc.Handler.EVENT_TOUCHES_MOVED )
        local eventDispatcher = layerTiles:getEventDispatcher()
        eventDispatcher:addEventListenerWithSceneGraphPriority(listener, layerTiles)

        cclog("TileMapEditTest tiles")
        local  map = cc.TileMapAtlas:create(s_TilesPng, s_LevelMapTga, 32, 32)
        -- Create an Aliased Atlas
        map:getTexture():setAliasTexParameters()

        local  s = map:getContentSize()
        cclog("ContentSize: %f, %f", s.width,s.height)

        -- If you are not going to use the Map, you can free it now
        -- [tilemap releaseMap)
        -- And if you are going to use, it you can access the data with:
        local function updateMap(dt)
            -- IMPORTANT
            --   The only limitation is that you cannot change an empty, or assign an empty tile to a tile
            --   The value 0 not rendered so don't assign or change a tile with value 0

            local  tilemap = layerTiles:getChildByTag(kTagTileMap)

            --
            -- For example you can iterate over all the tiles
            -- using this code, but try to avoid the iteration
            -- over all your tiles in every frame. It's very expensive
            --    for(int x=0 x < tilemap.tgaInfo:width x++)
            --        for(int y=0 y < tilemap.tgaInfo:height y++)
            --            Color3B c =[tilemap getTileAt:local Make(x,y))
            --            if( c.r != 0 )
            --                --------cclog("%d,%d = %d", x,y,c.r)
            --            end
            --        end
            --    end

            -- NEW since v0.7
            local c = tilemap:getTileAt(cc.p(13,21))
            c.r = c.r + 1
            c.r = c.r % 50

            if( c.r==0) then
                c.r=1
            end
            -- NEW since v0.7
            tilemap:setTile(c, cc.p(13,21) )
        end

        local schedulerEntry = nil
        local function onNodeEvent(event)
            if event == "enter" then
                schedulerEntry = scheduler:scheduleScriptFunc(updateMap, 0.2, false)
            elseif event == "exit" then
                scheduler:unscheduleScriptEntry(schedulerEntry)
            end
        end

        layerTiles:registerScriptHandler(onNodeEvent)

        layerTiles:addChild(map, 0, kTagTileMap)

        map:setAnchorPoint( cc.p(0, 0) )
        map:setPosition( cc.p(-20,-200) )
        
        
--        map:setAnchorPoint( cc.p(0, 0.5) )
--        local scale = cc.ScaleBy:create(4, 0.8)
--        local scaleBack = scale:reverse()
--        local  seq = cc.Sequence:create(scale, scaleBack)
--        map:runAction(cc.RepeatForever:create(seq))
        
        cclog("TileMapEditTest done")
        return layerTiles
    end



    -- create menu
    local function createLayerMenu()
        local layerMenu = cc.Layer:create()

        local menuPopup, menuTools, effectID

        local function menuCallbackClosePopup()
            -- stop test sound effect
            cc.SimpleAudioEngine:getInstance():stopEffect(effectID)
            menuPopup:setVisible(false)
        end

        local function menuCallbackOpenPopup()
            -- loop test sound effect
            local effectPath = cc.FileUtils:getInstance():fullPathForFilename("effect1.wav")
            effectID = cc.SimpleAudioEngine:getInstance():playEffect(effectPath)
            menuPopup:setVisible(true)
        end

        -- add a popup menu
        local menuPopupItem = cc.MenuItemImage:create("menu2.png", "menu2.png")
        menuPopupItem:setPosition(0, 0)
        menuPopupItem:registerScriptTapHandler(menuCallbackClosePopup)
        menuPopup = cc.Menu:create(menuPopupItem)
        menuPopup:setPosition(origin.x + visibleSize.width / 2, origin.y + visibleSize.height / 2)
        menuPopup:setVisible(false)
        layerMenu:addChild(menuPopup)
        
        -- add the left-bottom "tools" menu to invoke menuPopup
        local menuToolsItem = cc.MenuItemImage:create("menu1.png", "menu1.png")
        menuToolsItem:setPosition(0, 0)
        menuToolsItem:registerScriptTapHandler(menuCallbackOpenPopup)
        menuTools = cc.Menu:create(menuToolsItem)
        local itemWidth = menuToolsItem:getContentSize().width
        local itemHeight = menuToolsItem:getContentSize().height
        menuTools:setPosition(origin.x + itemWidth/2, origin.y + itemHeight/2)
        layerMenu:addChild(menuTools)

        return layerMenu
    end

    -- play background music, preload effect
    local bgMusicPath = cc.FileUtils:getInstance():fullPathForFilename("background.mp3") 
--    cc.SimpleAudioEngine:getInstance():playMusic(bgMusicPath, true)
    local effectPath = cc.FileUtils:getInstance():fullPathForFilename("effect1.wav")
    cc.SimpleAudioEngine:getInstance():preloadEffect(effectPath)

    -- run
    local sceneGame = cc.Scene:create()

    --sceneGame:addChild(createLayerFarm())
    --sceneGame:addChild(createLayerMenu())
    --require "websocketlayer"
    --sceneGame:addChild(createLayerWebSocket())
    --require "TileMaps"
    sceneGame:addChild(TileMapEditTest())
	
	if cc.Director:getInstance():getRunningScene() then
		cc.Director:getInstance():replaceScene(sceneGame)
	else
		cc.Director:getInstance():runWithScene(sceneGame)
	end
end


local status, msg = xpcall(main, __G__TRACKBACK__)
if not status then
    error(msg)
end


