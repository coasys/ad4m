const withNextra = require("nextra")({
  theme: "nextra-theme-docs",
  themeConfig: "./theme.config.tsx",
});

module.exports = withNextra({
  images: { unoptimized: true },
  output: "export",
  trailingSlash: true,
  basePath: "/ad4m",
});
