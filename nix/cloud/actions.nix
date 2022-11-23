{
  cell,
  inputs,
}: {
  "hydra/ci" = {
    task = "ci";
    io = ''
      // This is a CUE expression that defines what events trigger a new run of this action.
      // There is no documentation for this yet. Ask SRE if you have trouble changing this.

      let github = {
        #input: "${cell.library.actionCiInputName}"
        #repo: "input-output-hk/hydra"
      }

      #lib.merge
      #ios: [
        #lib.io.github_push & github,
        { #lib.io.github_pr, github, #target_default: false },
      ]
    '';
  };
}
