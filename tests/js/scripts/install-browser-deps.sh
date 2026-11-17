#!/bin/bash
# Install the system libraries for Playwright's debian13-x64 Chromium, Firefox and WebKit
# builds on CI's Ubuntu 25.04 runner (integration-tests-sfu-browsers).
#
# Playwright has no 25.04 builds or package list. `install-deps` falls back to 24.04's list
# (libicu74, libavcodec60: absent on 25.04) and aborts on the first missing name. Debian 13's
# builds use the same ICU (76) and ffmpeg (7) as 25.04, and so does its package list, apart
# from the names in RENAME. A name 25.04 does not have is skipped, not fatal: the ldd check
# at the end lists any library still missing, and `SFU_BROWSERS_REQUIRED` fails the tests if a
# browser cannot start.
#
# Package list: playwright-core's native deps for "debian13-x64" (tools, chromium, firefox,
# webkit). Update it with playwright-core.
set -uo pipefail

PACKAGES="xvfb fonts-noto-color-emoji fonts-unifont libfontconfig1 libfreetype6 xfonts-scalable
fonts-liberation fonts-ipafont-gothic fonts-wqy-zenhei fonts-tlwg-loma-otf fonts-freefont-ttf
libasound2t64 libatk-bridge2.0-0t64 libatk1.0-0t64 libatspi2.0-0t64 libcairo2 libcups2t64
libdbus-1-3 libdrm2 libgbm1 libglib2.0-0t64 libnspr4 libnss3 libpango-1.0-0 libx11-6 libxcb1
libxcomposite1 libxdamage1 libxext6 libxfixes3 libxkbcommon0 libxrandr2 libavcodec61
libcairo-gobject2 libdbus-glib-1-2 libgdk-pixbuf-2.0-0 libgtk-3-0t64 libharfbuzz0b
libpangocairo-1.0-0 libx11-xcb1 libxcb-shm0 libxcursor1 libxi6 libxrender1 libxtst6
libsoup-3.0-0 gstreamer1.0-libav gstreamer1.0-plugins-bad gstreamer1.0-plugins-base
gstreamer1.0-plugins-good libegl1 libenchant-2-2 libepoxy0 libevdev2 libgles2 libglx0
libgstreamer-gl1.0-0 libgstreamer-plugins-base1.0-0 libgstreamer1.0-0 libgtk-4-1 libgudev-1.0-0
libharfbuzz-icu0 libhyphen0 libicu76 libjpeg62-turbo liblcms2-2 libmanette-0.2-0 libnotify4
libopengl0 libopenjp2-7 libopus0 libpng16-16t64 libproxy1v5 libsecret-1-0 libwayland-client0
libwayland-egl1 libwayland-server0 libwebp7 libwebpdemux2 libwoff1 libxml2 libxslt1.1 libatomic1
libevent-2.1-7t64 libavif16"

# Debian name -> Ubuntu name, where they differ.
declare -A RENAME=([libjpeg62-turbo]=libjpeg62)

wanted=()
for p in $PACKAGES; do
  p=${RENAME[$p]:-$p}
  # Not `grep -q`: it exits at the first match, and apt-cache's SIGPIPE fails the pipeline.
  if apt-cache policy "$p" 2>/dev/null | grep 'Candidate: [^(]' >/dev/null; then wanted+=("$p")
  else echo "skipped (not in this release): $p"; fi
done
sudo apt-get install -y -qq --no-install-recommends "${wanted[@]}" || exit 1

# Libraries the browsers still cannot load. ldd without the browsers' rpath also reports the
# libraries they ship themselves, so those are left out.
root=${PLAYWRIGHT_BROWSERS_PATH:-$HOME/.cache/ms-playwright}
shipped=$(find "$root" -name '*.so*' -printf '%f\n' | sort -u)
missing=$(find "$root" -type f \( -perm -u+x -o -name '*.so*' \) -print0 | xargs -0 ldd 2>/dev/null \
  | awk '/=> not found/ {print $1}' | sort -u | grep -vxF -f <(echo "$shipped") || true)
echo "libraries still missing: ${missing:-none}"
