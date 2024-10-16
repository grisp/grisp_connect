# Software Update Feature

## State Diagram

```mermaid
stateDiagram
  direction TB
  [*] --> init
  init --> ready: valide_sys = next_sys
  init --> updated: valide_sys != next_sys, boot_sys != next_sys 
  init --> validating: valide_sys != next_sys, boot_sys = next_sys 
  validating --> ready: logging.Validate()
  ready --> updating: logging.Start()
  updating --> updated: update written
  updated --> ready: logging.Cancel()
  validating --> wait_reboot: logging.Cancel() or system.Reboot()
  updated --> wait_reboot: system.Reboot()
  wait_reboot --> [*]: reboot
```

## Protocol

When the server connects, after the handshake with the `"update"` feture
enabled and if the device is already linked, it will always send a
`"logging.Progress"` notification. If not linked, it will send the notification
after a successfull `"LinkDevice"` request.

In addition, a `"logging.Progress"` notification will be sent for every state
change, and durring the state `"updating"`, it will periodically be sent with
progress information and in case there is an important warning regarding the
update process.