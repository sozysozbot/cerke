function parser(info)
{
	var res_arr = [];

	// 0 "^" 1 "#" 2 "h" 0
	var state = 0;

	var tmp = {};

	for(var cnt = 0; cnt < info.length; cnt++){
		if(/\s/.test(info[cnt])) {
			continue;
		}

		if(state === 0){
			if(info[cnt] === "^" || info[cnt] === "_") {
				tmp.isReversed = (info[cnt] === "_");
				state++;
				continue;
			}

			if(info[cnt] === "-") {
				res_arr[res_arr.length] = null; // empty slot
				tmp = {};
				
				state = 0; // explicit state0

				continue;
			}

			error("Parse error: unexpected `" + info[cnt] + "` while expecting `^` or `_`.");
			return null;
		}

		if(state === 1){
			var ch = conv_obj[info[cnt]];
			if(ch == null){
				error("Parse error: unexpected `" + info[cnt] 
					+ "` while expecting `!`, `0`, `1`, `2`, `3`, `4`, `5`, `6`, `7`, `8`, `#` or `$`.");
				return null;
			}

			tmp.txt = ch;
			state++;
			continue;
		}

		if(state === 2){
			if(info[cnt] === "k" || info[cnt] === "h") {
				tmp.isRed = (info[cnt] === "k");
				res_arr[res_arr.length] = JSON.parse(JSON.stringify(tmp));
				tmp = {};
				state = 0;
				continue;
			}
		}

		throw new Error("cannot happen");
	}
	return res_arr;
}
