module.exports = {
  config: {
    paths: {
      watched: ["../src", "src"],
      public: "../docs"
    },
    files: {
      javascripts: {
        joinTo: "app.js"
      }
    },
    plugins: {
      elmBrunch: {
        mainModules: ["src/Main.elm"],
        outputFolder: "../docs/"
      }
    }
  }
};
