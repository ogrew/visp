# Visp

<p align="center">
  <img src="https://github.com/user-attachments/assets/22a5fa59-f266-485c-aa3b-e83ddb992c7c" />
</p>

## Overview

`visp` is a minimal command-line wrapper around [ffmpeg](https://ffmpeg.org), written in [Common Lisp](https://common-lisp.net) for both educational and practical use.  
It simplifies repetitive `ffmpeg` operations like resolution conversion, audio stripping, codec switching, and more — all while auto-generating output filenames.

Originally created as a Lisp learning project, `visp` has grown into a practical tool that automates and streamlines common video encoding tasks.

## Usage Examples

```bash
# Downscale a 4K video to 1080p and remove audio
visp --input sample_4k.mp4 --res fhd --mute

# Specify resolution explicitly with width and height
visp --input clip.mp4 --res 1280x720

# Transcode to HAP codec, preserving resolution and audio
visp --input sample.mov --codec hap

# Preview the ffmpeg command without running it
visp --input sample.mov --res 2k --codec prores --dry-run

# Downscale the input video to half its original resolution
visp --input demo.mp4 --half

# Reverse the video (mute implied)
visp --input intro.mov --reverse

# Repeat input video 4 times (original + 3 loops)
visp --input loopclip.mp4 --loop 3

# Change playback speed to 2x
visp --input sample.mp4 --speed 2.0

# Change playback speed to half (0.5x)
visp --input sample.mp4 --speed 0.5

# Apply multiple options together
visp --input raw.mp4 --res hd --codec h265 --fps 24 --mute

# Specify custom output filename
visp --input source.mp4 --res fhd --output final_video.mp4

# Merge multiple mp4 videos into one
visp --merge intro.mp4 scene.mp4 outro.mp4

# Merge videos with custom output filename
visp --merge intro.mp4 scene.mp4 outro.mp4 --output compilation.mp4

# Convert a video to an animated GIF (fixed size, half fps)
visp --gif teaser.mp4

# Convert to GIF with custom output filename
visp --gif teaser.mp4 --output animation.gif

# Apply the same options to all videos in a directory (batch processing)
visp --input videos/ --mono --fps 24
```

Output filenames are automatically determined based on options.

Examples:

- `sample_fhd.mp4`
- `sample_720p_30fps_noSound.mov`
- `intro_noSound_Reverse.mp4`
- `loopclip_x4.mp4`
- `demo_Half.mp4`
- `sample_2xSpeed.mp4`
- `sample_0.5xSpeed.mp4`
- `teaser.gif`

## Options

| Option      | Argument                                      | Description                                                      |
| ----------- | --------------------------------------------- | ---------------------------------------------------------------- |
| `--input`   | `<file or directory>`                         | **Required.** Path to the input video file or directory.         |
| `--output`  | `<filename>`                                  | Custom output filename. If not specified, auto-generated based on options. |
| `--res`     | `hd`, `fhd`, `2k`, `4k`, `8k`, `<WxH>`        | Target resolution (e.g. `fhd` or `1920x1080`).                   |
| `--codec`   | `h264`, `h265`, `prores`, `hap`, `vp8`, `vp9` | Video codec (also determines container and pixel format).        |
| `--fps`     | `<number>`                                    | Set output framerate (e.g. 24, 30, 60).                          |
| `--mute`    | _(flag)_                                      | Strip out the audio track.                                       |
| `--loop`    | `<number>`                                    | Loop the input video N times (e.g. `--loop 4` → repeat 3 times). |
| `--reverse` | _(flag)_                                      | Reverse the input video (audio is automatically muted).          |
| `--mono`    | _(flag)_                                      | Convert video to grayscale (supported only with `h264`/`h265`).  |
| `--half`    | _(flag)_                                      | Scale input resolution down by half (e.g. 1920×1080 → 960×540).  |
| `--speed`   | `<factor>`                                    | Change playback speed (e.g. `2.0` = 2x speed, `0.5` = half speed). |
| `--dry-run` | _(flag)_                                      | Print the generated `ffmpeg` command without executing it.       |
| `--help`    | _(flag)_                                      | Show usage information and exit.                                 |

**Note:**  
If a directory is specified with `--input`, all video files in the directory will be processed in batch.
The generated output files will be saved in the same directory as each input file.

## Merge Mode

You can also use `visp` to merge multiple `.mp4` videos into one.

```bash
visp --merge part1.mp4 part2.mp4 part3.mp4
```

Requirements:

- All input files must be `.mp4`.
- At least two files are required.
- Files must either all have audio, or all be silent (no mix).
- All videos will be normalized to the **resolution and framerate** of the first file.
- The output file will be named after the first file with `_merged.mp4` appended, or use `--output` to specify a custom filename.
- `--dry-run` and `--output` may be combined to preview the command or customize the output filename.

Example:

```bash
visp --merge clipA.mp4 clipB.mp4 --dry-run
```

Sample output:

```text
[WARN] Detected different resolutions across files. All will be scaled to 1920x1080.
[WARN] Detected different fps values. All will be converted to 60.0fps.
[INFO] Planned output file: clipA_merged.mp4
[DRY-RUN] Command: ffmpeg -i clipA.mp4 -i clipB.mp4 -filter_complex "[0:v:0]scale=1920:1080,fps=60[v0];[1:v:0]scale=1920:1080,fps=60[v1];[0:a:0][1:a:0]concat=n=2:v=1:a=1[outv][outa]" -map "[outv]" -map "[outa]" -c:v libx264 -c:a aac -y clipA_merged.mp4
```

## GIF Mode

You can convert a single video file into an animated GIF using `visp`.

```bash
visp --gif input.mp4
```

Details:

- Only one input file is allowed.
- The output resolution is fixed to **640 pixels wide**, height is auto-scaled to preserve aspect ratio.
- The output framerate is set to **half of the original video’s fps**.
- The output file will be named after the input with `.gif` as the extension, or use `--output` to specify a custom filename.
- Other options (e.g. `--mute`, `--res`, etc.) **cannot be combined** with `--gif`, except `--output` and `--dry-run`.
- `--dry-run` may be used to preview the ffmpeg command.
- A high-quality palette is generated with `palettegen`, and dithering is applied using `dither=bayer:bayer_scale=3:diff_mode=rectangle` for optimal compression and visual fidelity.

Example:

```bash
visp --gif teaser.mp4 --dry-run
```

Sample output:

```text
[INFO] Planned output file: teaser.gif
[DRY-RUN] Command: ffmpeg -i teaser.mp4 -filter_complex [0:v] fps=15.00,scale=640:-1,split [a][b];[a] palettegen=stats_mode=single [p];[b][p] paletteuse=dither=bayer:bayer_scale=3:diff_mode=rectangle:new=1 -y teaser.gif
```

## Install

The easiest way to install **visp** is now via Homebrew! 🍻

> **Note:** visp currently supports **Apple Silicon (arm64) Macs** only.

### Install with Homebrew (Recommended)

Step 1. Tap the repository:

```bash
brew tap ogrew/visp
```

Step 2. Install visp:

```bash
brew install visp
```

Step 3. Verify installation:

```bash
visp --help
```

That's it! Now you can use `visp` from anywhere.

### Build from source (Alternative)

If you prefer building manually or are not using Homebrew, you can build visp from source.

Requires [Roswell](https://github.com/roswell/roswell) and [SBCL](http://www.sbcl.org).

```bash
git clone https://github.com/ogrew/visp.git
cd visp
ros build visp.ros
```

Move the generated binary into a directory included in your `$PATH` (e.g., `/usr/local/bin`):

```bash
sudo mv visp /usr/local/bin/
sudo chmod +x /usr/local/bin/visp
```

Then you can use it like this:

```bash
visp --help
```

## TODO

Planned features and improvements for future versions of `visp`:

- 🖼 Image sequence to video: Support for turning numbered images (e.g., `%04d.png`) into a single video.
- 🧩 Multi-input tiling: Combine up to 4 videos into a 2×2 tiled layout.

## Requirements

`ffmpeg` must be installed and available in your system `PATH`.

To confirm:

```bash
which ffmpeg
# /opt/homebrew/bin/ffmpeg

ffmpeg -version
# ffmpeg version 7.1.1 ...
```

If `ffmpeg` is not detected, `visp` will show an error and terminate early.

Developer environment used:

```
$ ffmpeg -version
ffmpeg version 7.1.1 Copyright (c) 2000-2025 the FFmpeg developers
built with Apple clang version 16.0.0 (clang-1600.0.26.6)
```

## License

[MIT License](https://github.com/ogrew/visp/blob/main/LICENSE)
