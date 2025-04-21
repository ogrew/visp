# Visp

## Overview

**Visp** is a minimal command-line wrapper around [ffmpeg](https://ffmpeg.org), written in [Common Lisp](https://common-lisp.net) for both educational and practical use.  
It simplifies repetitive `ffmpeg` operations like resolution conversion, audio stripping, codec switching, and more — all while auto-generating output filenames.

Originally created as a Lisp learning project, **Visp** has grown into a practical tool that automates and streamlines common video encoding tasks.

## Usage Examples

```bash
# Downscale a 4K video to 1080p and remove audio
./visp --input sample_4k.mp4 --res fhd --mute

# Transcode to HAP codec, preserving resolution and audio
./visp --input sample.mov --codec hap

# Preview the ffmpeg command without running it
./visp --input sample.mov --res 2k --codec prores --dry-run
```

Output filenames are automatically determined based on options.

Examples:

- `sample_fhd.mp4`
- `sample_720p_30fps_noSound.mov`
- `sample_4k_noSound.mp4`

## Options

| Option      | Argument                        | Description                                                |
| ----------- | ------------------------------- | ---------------------------------------------------------- |
| `--input`   | `<file>`                        | **Required.** Path to the input video file.                |
| `--res`     | `hd`, `fhd`, `2k`, `4k`, `8k`   | Target resolution (e.g. `fhd` → 1920×1080).                |
| `--codec`   | `h264`, `h265`, `prores`, `hap` | Video codec (and container) to use.                        |
| `--fps`     | `<number>`                      | Set output frame rate (e.g. 24, 30, 60).                   |
| `--mute`    | _(flag)_                        | Strip out the audio track.                                 |
| `--dry-run` | _(flag)_                        | Print the generated `ffmpeg` command without executing it. |
| `--help`    | _(flag)_                        | Show usage information and exit.                           |

## Build & Setup

Requires [Roswell](https://github.com/roswell/roswell) and [SBCL](http://www.sbcl.org)

```bash
git clone https://github.com/ogrew/visp.git
cd visp
ros build visp.ros
chmod +x visp
```

This will generate a single binary executable called visp.

## Requirements

`ffmpeg` must be installed and available in your system `PATH`.

To confirm:

```bash
which ffmpeg
# /opt/homebrew/bin/ffmpeg

ffmpeg -version
# ffmpeg version 7.1.1 ...
```

If `ffmpeg` is not detected, **Visp** will show an error and terminate early.

Developer environment used:

```
$ ffmpeg -version
ffmpeg version 7.1.1 Copyright (c) 2000-2025 the FFmpeg developers
built with Apple clang version 16.0.0 (clang-1600.0.26.6)
```

## License

[MIT License](https://github.com/ogrew/visp/blob/main/LICENSE)
