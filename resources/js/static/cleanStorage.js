var timestamp_digits = ((new Date()).getTime()/1000).toFixed().length

function cleanStorage() {
  var keys_and_times = [];
  for (k in localStorage) {
    var saved = localStorage.getItem(k);
    var saved_time = saved.substring(0, timestamp_digits);
    keys_and_times.push([saved, k]);
  }

  var oldest_keys_first = keys_and_times.sort();

  for (i = 0; i <= (oldest_keys_first.length / 2) - 1; i++) {
    k = oldest_keys_first[i][1];
    localStorage.removeItem(k);
  }
}
