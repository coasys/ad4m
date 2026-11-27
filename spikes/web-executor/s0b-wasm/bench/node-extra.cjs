const path=require('path'),fs=require('fs');
const m=require(path.resolve(process.argv[2],'s0b_oxigraph.js'));
m.reset(); m.load_links(50000,false); m.prepare_queries();
const q2=m.query_text(1); const ids=q2.match(/VALUES \?source \{([^}]*)\}/)[1].trim();
for (const q0 of fs.readFileSync(process.argv[3],'utf8').split('\n----\n').filter(x=>x.trim())) {
  const q=q0.replace('__VALUES__',ids); const rows=m.run_sparql(q);
  const t=performance.now(); for(let i=0;i<5;i++) m.run_sparql(q);
  console.log(`extra\trows=${rows}\tavg_ms=${((performance.now()-t)/5).toFixed(3)}\t${q0.slice(0,50)}`);
}
