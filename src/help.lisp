(in-package :visp)

(defun print-help ()
  (format t "=================================================~%")
  (format t "~%visp - minimal ffmpeg wrapper written in Common Lisp~%~%")
  (format t "=================================================~%")

  ;; Usageセクション
  (format t "Usage:~%")
  (format t "  visp --input <file> [options]    ;; Normal mode~%")
  (format t "  visp --merge <file1> <file2> ... ;; Merge multiple mp4 files into one~%")
  (format t "  visp --gif <file>                ;; Convert a single video to GIF~%~%")

  ;; Normal mode options
  (format t "Normal mode options:~%")
  (format t "  --input <file>        Input video file (required)~%")
  (format t "  --res <label>         Set resolution (hd, fhd, 2k, 4k, etc)~%")
  (format t "  --half                Downscale to half resolution~%")
  (format t "  --fps <number>        Set output framerate (e.g., 30)~%")
  (format t "  --codec <type>        Set codec (h264, h265, prores, hap, vp8, vp9)~%")
  (format t "  --mono                Convert to grayscale (h264/h265 only)~%")
  (format t "  --reverse             Reverse video playback (audio unaffected)~%")
  (format t "  --loop <n>            Loop playback n times (e.g., 3 = 4 total plays)~%")
  (format t "  --mute                Remove audio track~%")
  (format t "  --dry-run             Show ffmpeg command without executing~%")
  (format t "  --help                Show this help message~%~%")

  ;; Merge mode explanation
  (format t "Merge mode (exclusive):~%")
  (format t "  --merge <file1> <file2> ...~%")
  (format t "    Merge multiple .mp4 videos into one output file.~%")
  (format t "    - All files must be .mp4~%")
  (format t "    - At least two files required~%")
  (format t "    - All files must have matching audio presence (either all with or all without)~%")
  (format t "    - Output file will be named based on the first file + _merged.mp4~%")
  (format t "    - Resolution and framerate will be normalized to the first file~%")
  (format t "    - Other options cannot be combined with --merge~%")
  (format t "    - However, --dry-run can be combined to preview the ffmpeg command~%~%")

  ;; GIF mode explanation
  (format t "GIF mode (exclusive):~%")
  (format t "  --gif <file>~%")
  (format t "    Convert video to GIF animation.~%")
  (format t "    - Only one input file allowed~%")
  (format t "    - Resolution is fixed to 640px wide, height auto-scaled~%")
  (format t "    - Output framerate is half of the source video's framerate~%")
  (format t "    - Output file will be named like input.gif (e.g., movie.mp4 → movie.gif)~%")
  (format t "    - Other options cannot be combined with --gif (except --dry-run)~%")
  (format t "    - High-quality palette and Bayer dithering applied for compression~%~%"))
