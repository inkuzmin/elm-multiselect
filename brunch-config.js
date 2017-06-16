module.exports = {
  config: {
    paths: {
      watched: ["app"],
      public: "public"
    },
    files: {
      javascripts: {
        joinTo: "js/app.js"
      },
      stylesheets: {
        joinTo: "css/app.css"
      }
    },
    plugins: {
      elmBrunch: {
        mainModules: ["app/elm/Main.elm"],
        outputFolder: "public/js/"
      },
      elmCss: {
        sourcePath: ["app/elm/Stylesheets.elm"],
        outputDir: "public/css/"
      }
    }
  }
};
