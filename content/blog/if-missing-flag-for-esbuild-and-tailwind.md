---
title: "--if-missing flag for esbuild and tailwind installers"
date: "2022-02-27T18:08:13.265Z"
categories: ["phoenix", "dependencies", "build tools"]
---

If you work on any Phoenix applications that leverage the standalone versions of [esbuild](https://github.com/phoenixframework/esbuild) or [Tailwind](https://github.com/phoenixframework/tailwind) you are likely familiar with the method for installing binaries for these tools:

```bash
mix tailwind.install
mix esbuild.install
```

For projects that include [setup scripts](https://github.com/github/scripts-to-rule-them-all), you've likely even updated one of your scripts to include this step. If you have, you may have noticed you are re-downloading the binaries every time you install them.

```bash
› mix esbuild.install

06:45:32.664 [debug] Downloading esbuild from https://registry.npmjs.org/esbuild-darwin-64/-/esbuild-darwin-64-0.14.0.tgz
```

Fortunately, both the `tailwind` and `esbuild` packages included a flag, `--if-missing`. With this flag set, it will only fetch the binary if you do not already have it (even accounting for the version!)

```bash
# first install
› mix esbuild.install --if-missing

06:45:32.664 [debug] Downloading esbuild from https://registry.npmjs.org/esbuild-darwin-64/-/esbuild-darwin-64-0.14.0.tgz

# try again, nothing is downloaded
› mix esbuild.install --if-missing

# updated config to bump esbuild vesion
› mix esbuild.install --if-missing

06:53:23.520 [debug] Downloading esbuild from https://registry.npmjs.org/esbuild-darwin-64/-/esbuild-darwin-64-0.14.1.tgz
```

This flag is not noted in the `README`, so you may have missed it. For this, and other flags, you can check the HexDocs for [`tailwind`](https://hexdocs.pm/tailwind/Mix.Tasks.Tailwind.Install.html#content) and [`esbuild`](https://hexdocs.pm/esbuild/Mix.Tasks.Esbuild.Install.html#content).
