---
title: "--if-missing flag for standalone CLI wrappers"
date: "2022-04-30T08:08:13.265Z"
categories: ["phoenix", "dependencies", "build tools"]
---

If you work on any Phoenix applications that leverage the standalone versions of build tools like [esbuild](https://github.com/phoenixframework/esbuild), [Tailwind](https://github.com/phoenixframework/tailwind), or [Dart Sass](https://hexdocs.pm/dart_sass/DartSass.html), you are likely familiar with the method for installing binaries for these tools:

```bash
mix tailwind.install
mix esbuild.install
mix sass.install
```

For projects that include [setup scripts](https://github.com/github/scripts-to-rule-them-all), you've likely even updated one of your scripts to include this step. If you have, you may have noticed you are re-downloading the binaries every time you install them.

```bash
› mix esbuild.install

06:45:32.664 [debug] Downloading esbuild from https://registry.npmjs.org/esbuild-darwin-64/-/esbuild-darwin-64-0.14.0.tgz
```

Fortunately, the install scripts for the packages include an `--if-missing` flag. With this flag set, it will only fetch the binary if you do not already have it (even accounting for the version!)

```bash
# first install
› mix esbuild.install --if-missing

06:45:32.664 [debug] Downloading esbuild from https://registry.npmjs.org/esbuild-darwin-64/-/esbuild-darwin-64-0.14.0.tgz

# try again, nothing is downloaded
› mix esbuild.install --if-missing

# updated config to bump esbuild version
› mix esbuild.install --if-missing

06:53:23.520 [debug] Downloading esbuild from https://registry.npmjs.org/esbuild-darwin-64/-/esbuild-darwin-64-0.14.1.tgz
```

For documentation on this and other install flags, you can check the project's "Mix Tasks" section in HexDocs. Here are the links for [`tailwind`](https://hexdocs.pm/tailwind/Mix.Tasks.Tailwind.Install.html#content), [`esbuild`](https://hexdocs.pm/esbuild/Mix.Tasks.Esbuild.Install.html#content), and [`dart_sass`](https://hexdocs.pm/dart_sass/Mix.Tasks.Sass.Install.html).
