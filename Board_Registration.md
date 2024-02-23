
# Register Your GRiSP2 Board with GRiSP.io

Welcome! This guide walks you through linking your GRiSP2 board to your GRiSP.io account. Begin with obtaining a unique linking token, then choose between automatic or manual registration based on your preference.

## Obtain Your Device Linking Token

To start, you'll need a unique token:

1. **Log In or Sign Up:** Navigate to [GRiSP.io](https://grisp.io) and access your account or create a new one.
2. **Get Your Token:** Visit the [GRiSP Manager](https://grisp.io/grisp-manager/) to request a new Linking Token by clicking on **Link Device**. This token is crucial for the next steps in both automatic and manual registration methods.

---

With your token ready, select one of the following options to register your GRiSP2 board:

## Option 1: Automatic Linking with grisp_auto_link

For a hassle-free experience:

- **Download the Binary Package:** Look for the [grisp_auto_link](https://github.com/grisp/grisp_auto_link) zip file on the GRiSP Manager page under the **Link Device** section. It comes pre-configured with your linking token.
- **Prepare Your SD Card:** Extract the zip file onto a Fat32 formatted SD card and insert it into your GRiSP2 board.
- **Initiate Automatic Registration:** Connect your board to the internet via an ethernet cable. It will automatically link to your GRiSP.io account, which you can verify on the GRiSP Manager.

## Option 2: Manual Linking (For Advanced Users)

If you prefer a hands-on approach or need custom setup:

1. **Project Setup:** Follow the [GRiSP Wiki](https://github.com/grisp/grisp/wiki) for instructions on creating and deploying your GRiSP2 application. Before proceeding, ensure `grisp_io` is added to your project's dependencies. This is a crucial step for manual linking.

   - **Optional Configuration:** To streamline the process, include the device_linking_token in your project's release configuration (`config/sys.config`):
    ```erlang
    [
        {grisp_io, [
            {device_linking_token, <<"your_token_here">>}
        ]}
    ].
    ```

    This allows you to skip manually entering the token in the future.

2. **Deploy Your Application:** After setting up, make sure your board is connected to the internet. You can verify that your board connected to GRiSP.io executing:
   ```erlang
   > grisp_io:is_connected().
   true
   ```

3. **Manually Link Your Board:** Access the Erlang shell on your board. To link, execute:
   ```erlang
   > grisp_io:link_device(<<"your_unique_token">>).
   ```
   Or, if your token is pre-configured in the environment, run:
   ```erlang
   > grisp_io:link_device().
   ```

4. **Check Registration Status:** A successfull request returns `{ok, <<"ok">>}`. Confirm your device's registration on the [GRiSP Manager](https://grisp.io/grisp-manager/) page. Your device should appear under the **Devices** section. :tada:

### Troubleshooting:
`grisp_io:link_device/*` may fail with the following errors.
#### **Common Errors:**
- `token_expired`: regenerate one from the web page
- `invalid_token`: please double check you typed it correctly
- `token_undefined`: you called `grisp_io:link_device/0` without setting `device_linking_token`
- `disconnected`: check that your board can connect
- `device_already_linked`: please do not steal GRiSP boards :smirk:
  if you need to unlink a GRiSP board see below...


## Need to Unlink a Device?

We currently do not expose a public API to unlink a Device. Please reach out to us for assistance.

If you encounter any problems or have questions, don't hesitate to contact [support](mailto:grisp@stritzinger.com). Happy coding!
