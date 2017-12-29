module.exports = {
  config: {
    paths: {
      watched: ["../src", "src"],
      public: "../docs"
    },
    files: {
      javascripts: {
        joinTo: "app.js"
      },
      stylesheets: {
        joinTo: "app.css"
      }
    },
    plugins: {
      elmBrunch: {
        mainModules: ["../src/Multiselect.elm", "src/Main.elm"],
        outputFolder: "../docs/"
      },
      elmCss: {
        sourcePath: ["../src/Multiselect/Stylesheets.elm"],
        outputDir: "../docs/"
      }
    }
  }
};
