let siri;
let siriInstance  = false;

core.Siriwave = async (args, env) => {
  const op = await interpretate(args[0], env);
  const id = await interpretate(args[1], env);
  const doc = document.getElementById(id);

  if (!siri) siri = (await import('./siriwave.esm-80419990.js')).default;

  switch(op) {
    case 'Start':
      if (siriInstance) return;
      siriInstance = new siri({
        container: doc,
        autostart: true,
        style: "ios9",
        amplitude:10,
        curveDefinition: [
          { color: "255,255,255", supportLine: true },
          { color: "15, 82, 255" }, // blue
          { color: "255, 57, 76" }, // red
          { color: "48, 220, 0" }, // green
      ]
      });
    break;

    case 'Stop':
      if (!siriInstance) return;
      siriInstance.setAmplitude(5);
      const u = siriInstance;
      setTimeout(() => u.dispose(), 2000);
      siriInstance = false;
    break;
  }
};

core.ReadClipboardExtended = async (args, env) => {
  const clipboardContents = await navigator.clipboard.read();
  for (const item of clipboardContents) {

    const data = new Uint8Array(await (await item.getType(item.types[0])).arrayBuffer());
    const type = item.types[0];
    return [type, Array.from(data)];
  }
};
