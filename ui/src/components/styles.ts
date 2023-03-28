export const MainHeader = {
  width: "calc(100% - 40px)",
  background: "#000",
  display: "flex",
  justifyContent: "space-between",
  padding: `20px`,
  fontFamily: "comfortaa",
};

export const cardStyle = {
  width: "100%",
  border: "1px solid var(--j-color-ui-200)",
  borderRadius: "var(--j-border-radius)",
  backgroundColor: "var(--j-color-ui-50)",
  padding: "var(--j-space-500)",
  marginBottom: "var(--j-space-500)",
  position: "relative",
};

export const linkStyle = {
  fontSize: "var(--j-font-size-400)",
  color: "var(--j-color-primary-600)",
  textDecoration: "underline",
};

export const listStyle = {
  overflowY: "scroll",
  padding: 20,
  overflowX: "hidden",
  marginTop: 10,
};

export const splashscreenError = {
  padding: "40px 80px",
  visibility: "collapse",
  opacity: 0,
  transition: "visibility 0s, opacity 0.5s linear, height 1s",
  height: 0,
}

export const splashscreenErrorFlex = {
  display: "flex",
  flexDirection: "column",
  alignItems: "center",
}

export const splashscreenContainer = {
  display: "flex",
  alignItems: "center",
  justifyContent: "center",
  flexDirection: "column",
  height: "100vh",
  width: "100vw",
  color: "white",
}