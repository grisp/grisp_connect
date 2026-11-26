# Release instructions

## Before a release

1. Make sure that [release.yml](.github/workflows/release.yml) is on the default branch.
2. Use a GitHub CLI account with write access to `grisp/grisp_connect`.
3. Make sure that the tests pass for the commit on the default branch.
4. Make sure that Hex Workload Identity uses `grisp/grisp_connect`, `release.yml`, and the `release` environment.

The GitHub `release` environment supplies the `RELEASE_SIGNING_KEY` secret and the `RELEASE_SIGNING_EMAIL` variable.
The placeholders for these values are `<PRIVATE_GPG_KEY>` and `<GPG_KEY_EMAIL>`.
For a protected key, the `RELEASE_GPG_KEY_PASSPHRASE` secret supplies the passphrase.
The passphrase placeholder is `<GPG_KEY_PASSPHRASE>`.

## Send a release request

1. Replace `X.Y.Z` with the version that the user gives.
2. Run the following command.

   ```sh
   gh api --method POST repos/grisp/grisp_connect/dispatches --input - <<'JSON'
   {"event_type":"release","client_payload":{"version":"X.Y.Z"}}
   JSON
   ```

3. Open the new **Release** run in [GitHub Actions](https://github.com/grisp/grisp_connect/actions/workflows/release.yml).
4. Get the Hex approval link from the **Publish to Hex** step log.
5. For a documentation upload, use the **Publish the Hex documentation** step log.
6. Give the approval link to the person who has approval access on Hex.

Hex approval requires two-factor authentication.
The workflow creates the GitHub release before it stages the package and documentation on Hex.
The job ends after the upload. Hex approval can occur after the job ends.

## Continue a release

If Hex waits for approval after the job ends, use the approval link in the run log.
If the run fails after it pushes the tag, correct the cause of the failure.
Select **Re-run all jobs** on the original run.
The workflow makes sure that the tag is for that run.
