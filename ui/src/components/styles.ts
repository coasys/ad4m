export const MainContainer = {
  width: "100%",
  position: "relative" as "relative",
  padding: 0,
  maxWidth: "100%",
  overflowX: "clip" as "clip",
  margin: 0,
};

export const MainHeader = {
  width: "calc(100% - 40px)",
  background: "#000",
  display: "flex",
  justifyContent: "space-between",
  padding: `20px`,
  fontFamily: "comfortaa",
};

export const RouteContainer = {
  width: "100%",
  overflowX: "hidden" as "hidden",
  maxWidth: "calc(100%-320px)",
};

export const Header = {
  padding: "20px 30px 10px 30px",
};

export const cardStyle = {
  width: "100%",
  display: "flex",
  alignItems: "center",
  gap: "var(--j-space-500)",
  borderRadius: "var(--j-border-radius)",
  textDecoration: "none",
  backgroundColor: "var(--j-color-ui-50)",
  padding: "var(--j-space-500)",
  marginBottom: "var(--j-space-500)",
  position: "relative",
};

export const listStyle = {
  overflowY: "scroll",
  padding: 20,
  overflowX: "hidden",
  marginTop: 10,
};

export const gridButton = {
  display: "grid",
  columnGap: "calc(100% - 305px)",
  rowGap: "30px",
  gridTemplateColumns: "repeat(3, 79px)",
  justifyContent: "center",
  alignItems: "center",
  padding: "30px 20px 0 20px",
};

export const badge = {
  width: "fit-content",
  padding: "4px 12px",
  background: "#845EF7",
  borderRadius: 30,
  color: "var(--j-color-black)",
  fontSize: "var(--j-font-size-400)",
};
