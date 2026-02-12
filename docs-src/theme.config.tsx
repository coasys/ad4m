import { useConfig } from "nextra-theme-docs";
import { useRouter } from "next/router";
import { version } from "../core/package.json";

export default {
  project: {
    link: "https://github.com/perspect3vism/ad4m",
  },
  logo: (
    <>
      <img src="/ad4m/images/logo.svg" style={{ marginRight: "10px" }} />
      AD4M Docs {version}
    </>
  ),
  darkMode: true,
  docsRepositoryBase: "https://github.com/perspect3vism/ad4m/blob/dev/docs/",
  footer: {
    text: "AD4M - The first social network",
  },
  search: {
    loading: "Loading...",
  },
  head: function useHead() {
    const config = useConfig<{ description?: string; image?: string }>();
    const description =
      config.frontMatter.description || "Documentation site for AD4M";
    const image =
      config.frontMatter.image ||
      "https://docs.ad4m.dev/images/ad4m-og-card.jpg";
    return (
      <>
        <meta httpEquiv="Content-Language" content="en" />
        <meta name="description" content={description} />
        <meta name="og:description" content={description} />
        <meta name="twitter:card" content="summary_large_image" />
        <meta name="twitter:site" content="@_a_d_4_m" />
        <meta name="twitter:image" content={image} />
        <meta name="og:title" content={`${config.title} – AD4m Docs`} />
        <meta name="og:image" content={image} />
        <meta name="apple-mobile-web-app-title" content="AD4m Docs" />
        <link
          href="/favicon-dark.ico"
          rel="icon"
          type="image/x-icon"
          media="(prefers-color-scheme: light)"
        />
        <link
          href="/favicon-light.ico"
          rel="icon"
          type="image/x-icon"
          media="(prefers-color-scheme: dark)"
        />
      </>
    );
  },
  useNextSeoProps() {
    return {
      titleTemplate: `%s | AD4M Docs`,
    };
  },
};
