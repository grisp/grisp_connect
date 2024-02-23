# How To link your GRiSP2 Board to your GRiSP.io account

## Get yourself a GRiSP.io device linking token


1. Login or Register on [GRiSP.io](https://grisp.io)
2. Request a new Linking Token on the [GRiSP Manager](https://grisp.io/grisp-manager/) web view
3. Use the user-friendly binary application or try to manually link your board as explained below

## Use the grisp_auto_link binary package

If you do not want to write code we provide a zip file that can be extracted on a Fat32 SD card.
This zip contains a deployed release of [grisp_auto_link](https://github.com/grisp/grisp_auto_link) application,
customized with your linking token. You then just need to put the SD card in your board and connect it with an ethernet cable to the internet. The application will connect the board for you and you will be able to see it in the GRiSP Manager web view. You will find the download button near your token on GRiSP Manager the webpage.

## Manually link your board

You are free to use the grisp_io API in your code or procede manually while connected by serial or remote shell to the erlang shell running on the board.

1. Follow the [GRiSP Wiky](https://github.com/grisp/grisp/wiki)
to create your GRiSP2 application project and learn how to deploy

    - **Optional:** if you do not want to type the device_linking_token at runtime
you can add it to the application env variables in your release confi. (e.g. `config/sys.config`)
Copy in there the Linking Token you precedently generated on the web page.
```
[
    {grisp_io, [
        {device_linking_token, <<"...">>}
    ]}
].
```

2. Follow the tutorial until you are able to deploy your app on the board.


By default grisp_io application connects on boot, it will keep tryng until it succedes.

3. Make sure your board is able to connect to the internet
4. Connect via serial or remote_shell to the erlang shell on the board.
5. In the erlang shell: verify connection to GRiSP.io
```
    > grisp_io:is_connected().
    true
```
6. Check if your board is registered or not. `pong` means that is already linked,
`pang` means that is not linked with any account.
```
    > grisp_io:ping().
    {ok, <<"pang">>}
```
7. Send a linking request providing the Linking Token as binary argument in the call.
```
    > grisp_io:link_device(<<"my_long_and_unique_token">>).
    {ok, <<"ok">>}
```
- If you did set the token in the grisp_io env you can just use the shorter call.
```
    > grisp_io:link_device().
    {ok, <<"ok">>}
```
8. Checkout [GRiSP Manager](https://grisp.io/grisp-manager/) device view to spot
your device. Now you will be able to monitor its activities from GRiSP.io

Possible errors during linking:
- `token_expired`: regenerate one from the web page
- `invalid_token`: please double check you typed it correctly
- `token_undefined`: you called `grisp_io:link_device/0` without setting `device_linking_token`
- `disconnected`: check that your board can connect
- `device_already_linked`: please do not steal GRiSP boards :smirk:
  if you need to unlink a GRiSP board see below...

## Unlink a Device

We currently do not expose a public API to unlink a GRiSP device.
If you need to unlink your board please contact: grisp@stritzinger.com
